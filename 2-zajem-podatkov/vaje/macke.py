import requests
import re
import os
import csv

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definiratje URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = (
    'http://www.bolha.com/zivali/male-zivali/macke/'
    )
# mapa, v katero bomo shranili podatke
cat_directory = 'cat_data'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'frontpage.html'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'cat_data.csv'


def download_url_to_string(url):
    '''This function takes a URL as argument and tries to download it
    using requests. Upon success, it returns the page contents as string.'''
    try:
        r = requests.get(url)
    except requests.exceptions.ConnectionError:
        print('Could not access page ' + url)
        # koda, ki se izvede pri napaki
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        return ''
    # nadaljujemo s kodo če ni prišlo do napake
    return r.text


def save_string_to_file(text, directory, filename):
    '''Write "text" to the file "filename" located in directory "directory",
    creating "directory" if necessary. If "directory" is the empty string, use
    the current directory.'''
    os.makedirs(os.path.join(os.path.dirname(__file__), directory), exist_ok=True)
    path = os.path.join(os.path.dirname(__file__), directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None

# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage(url):
    '''Save "cats_frontpage_url" to the file
    "cat_directory"/"frontpage_filename"'''
    tekst = download_url_to_string(url)
    return save_string_to_file(tekst, cat_directory, frontpage_filename)


###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
    '''Return the contents of the file "directory"/"filename" as a string.'''
    path = os.path.join(os.path.dirname(__file__), directory, filename)
    with open(path, encoding='utf-8') as datoteka:
        tekst = datoteka.read()
    return tekst


# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.


def page_to_ads(directory, filename):
    '''Split "page" to a list of advertisement blocks.'''
    vzorec = re.compile(
        r'<div class="(ad|ad featured)">'
        r'.*?'
        r'<div class="clear"></div>',
        re.DOTALL
        )
    vsebina = read_file_to_string(directory, filename)
    reklame = []
    for ujemanje in vzorec.finditer(vsebina):
        reklame.append(ujemanje.group(0))
    return reklame

# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, ceni in opisu v oglasu.


def get_dict_from_ad_block(oglas):
    '''Build a dictionary containing the name, description and price
    of an ad block.'''
    podatki_filmov = []
    vzorec = re.compile(
        r'<table><tr><td><a title="(?P<naslov>.+?)" href=.*?'
        r'</h3>\s+(?P<opis>.*?)\s+(<div|</div).*?'
        r'<div class="price">(<span>)?(?P<cena>.+?)(</span>)?</div>\s+.*?',
        re.DOTALL
    )
    podatki_filmov = []
    def pocisti_podatke(vsebina):
        podatki_filma = vsebina.groupdict()
        podatki_filma['opis'] = podatki_filma['opis'].strip()
        podatki_filma['naslov'] = podatki_filma['naslov'].strip().strip('"')
        podatki_filma['cena'] = (float(podatki_filma['cena'].rstrip(' €').replace(',','.')) if podatki_filma['cena'][-1] == '€' else podatki_filma['cena'])
        return podatki_filma
    for ujemanje in vzorec.finditer(oglas):
        podatki_filmov.append(pocisti_podatke(ujemanje))
    return podatki_filmov[0]

# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.

def ads_from_file(ime_datoteke, lokacija_datoteke):
    '''Parse the ads in filename/directory into a dictionary list.'''
    seznam_filmov = []
    seznam_reklam = page_to_ads(lokacija_datoteke, ime_datoteke)
    for reklama in seznam_reklam:
        seznam_filmov.append(get_dict_from_ad_block(reklama))
    return seznam_filmov

###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    '''Write a CSV file to directory/filename. The fieldnames must be a list of
    strings, the rows a list of dictionaries each mapping a fieldname to a
    cell-value.'''
    os.makedirs(os.path.join(os.path.dirname(__file__), directory), exist_ok=True)
    path = os.path.join(os.path.dirname(__file__), directory, filename)
    with open(path, 'w') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return None

# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.


def write_cat_ads_to_csv(seznam_slovarjev, lokacija_datoteke, ime_datoteke_csv):
    imena_kljucev = [i for i in seznam_slovarjev[0].keys()]
    write_csv(imena_kljucev, seznam_slovarjev, lokacija_datoteke, ime_datoteke_csv)
    return None

seznam = ads_from_file(frontpage_filename, cat_directory)
write_cat_ads_to_csv(seznam, cat_directory, csv_filename)
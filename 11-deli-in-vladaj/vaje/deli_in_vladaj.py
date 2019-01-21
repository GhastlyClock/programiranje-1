##############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. Ker bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ki se nahaja med
# indeksoma [start] in [end].
#
# Primer: za [start = 0] in [end = 8] tabelo
#
# [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
# [0, 2, 5, 4, 10, 11, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 10 pivot.)
#
# Sestavi funkcijo [pivot(a, start, end)], ki preuredi tabelo [a] tako, da bo
# element [ a[start] ] postal pivot za del tabele med indeksoma [start] in
# [end]. Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele [a].
#
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot(a, 1, 7)
#     3
#     >>> a
#     [10, 2, 0, 4, 11, 15, 17, 5, 18]
##############################################################################

# def pivot(a, start, end):
#     p = a[start] # Nastavim vrednost pivotnega elementa
#     indeks = start # Nastavim indeks, s katerim bom menjal kočni položaj pivota
#     for i in range(start + 1, end):
#         # Grem skozi seznam od pivota pa do konca in iščem elemente, ki so 'problematični'
#         if a[i] > p:
#             j = i + 1
#             while j <= end: # Nastavim pogoj za ustavitev
#                 if a[j] <= p:
#                     indeks = i
#                     a[i], a[j] = a[j], a[i]
#                     break
#                 j += 1
#     if indeks != start: # Menjam pivotni element z zadnjim različnim od pivota 
#         for i in range(indeks, start, -1):
#             if a[indeks] != p:
#                 a[start], a[indeks] = a[indeks], a[start]
#                 break
#     return indeks

# b = [10, 4, 5, 15, 11, 2, 17, 0, 18]
# # pivot(a, 1, 7)
# # print(a)

def pivot(a, start, end):
    piv = a[start]
    indeks = start + 1
    for i in range(indeks, end + 1):
        if a[i] < piv:
            a[indeks], a[i] = a[i], a[indeks]
            indeks += 1
    if indeks - 1 != start: # Menjam pivotni element z zadnjim različnim od pivota 
        for i in range(indeks - 1, start, -1):
            if a[indeks - 1] != piv:
                a[start], a[indeks - 1] = a[indeks - 1], a[start]
                break
    return indeks
a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
pivot(a, 1, 8)
print(a)

##############################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja (quicksort).
#
# Napišite funkcijo [quicksort(a)], ki uredi tabelo [a] s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: Definirajte pomožno funkcijo [quicksort_part(a, start, end)], ki
#        uredi zgolj del tabele [a].
#
#   >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#   >>> quicksort(a)
#   [2, 3, 4, 5, 10, 11, 15, 17, 18]
##############################################################################

def quicksort_part(a, start, end):
    if start < end:
        delitev = pivot(a, start, end)
        quicksort_part(a, start, delitev - 1)
        quicksort_part(a, delitev + 1, end)
    return a
    
a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
print(quicksort_part(a,0,8))

def quicksort(a):
    konc = len(a) - 1
    return quicksort_part(a, 0, konc)

a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
print(quicksort(a))

##############################################################################
# V tabeli želimo poiskati vrednost k-tega elementa po velikosti.
#
# Primer: Če je
#
# >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši elementi
#  2, 3 in 4. Pri tem štejemo indekse od 0 naprej, torej je "ničti" element 2.
#
# Sestavite funkcijo [kth_element(a, k)], ki v tabeli [a] poišče [k]-ti
# element po velikosti. Funkcija sme spremeniti tabelo [a]. Cilj naloge je, da
# jo rešite brez da v celoti uredite tabelo [a].
##############################################################################

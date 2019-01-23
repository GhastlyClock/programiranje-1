from functools import lru_cache

# Cilj: izračunajte vrednosti Fibonaccijevega zaporadja za 100, 500, 1000,
# 10**5, and 10**6 člen.
# Za vsako definicijo preizkusite kako pozne člene lahko izračuante in poglejte
# zakaj se pojavi problem (neučinkovitost, pregloboka rekurzija,
# premalo spomina ...).

# Definirajte naivno rekurzivno različico.
# Omejitev: Prepočasno.
def fib(n):
    if n <= 1:
        return n
    else:
        return fib(n-1) + fib(n-2)

# Z uporabo dekoratorja izboljšajte naivno različico.
# Omejitev: Preseže največjo dovoljeno globino rekurzija za ~350.
@lru_cache()
def fib_cache(n):
    if n < 1:
        return 1
    else:
        x = fib_cache(n-1)
        y = fib_cache(n-2)
        return x + y


# Nariši drevo klicov za navadno rekurzivno fib funkcijo pri n=5 in
# ugotovi kateri podproblemi so klicani večkrat.

# Definirajte rekurzivno memoizirano funkcijo fib brez uporabe dekoratorja.
# Omejitev: Preseže največjo dovoljeno globino rekurzija za ~1000.
def fib_memo_rec(n):
    fib_0_n = [None] * (max (2, n + 1))
    def aux (n):
        if n < 2:
            return (n)
        else:
            if fib_0_n[n] == None:
                fib_0_n[n] = aux(n-1) + aux(n-2)
            return fib_0_n[n]
    return aux(n)

# Na katere podprobleme se direktno skicuje rekurzivna definicija fib?

# Definirajte fib ki gradi rezultat od spodaj navzgor (torej računa in si zapomni
# vrednosti od 1 proti n.)
def fib_memo_iter(n):
    rezult = {}
    rezult[0] = 0
    rezult[1] = 1
    for i in range(2, n+1):
        rezult[i] = rezult[i-1] + rezult[i-2]
    return rezult[n]


# Izboljšajte prejšnjo različico tako, da hrani zgolj rezultate, ki jih v
# nadaljevanju nujno potrebuje.
def fib_iter(n):
    if n < 2:
        return n
    else:
        x_2 = 0
        x_1 = 1
        for i in range(2,n+1):
            x = x_1 + x_2
            x_2 = x_1
            x_1 = x
    return x

def fibonaqi(n):
    rezultati = {0:0, 1:1}
    def pomozna(n):
        if n not in rezultati:
            rezultati[n] = pomozna(n-1) + pomozna(n-2)
        return rezultati[n]
    return pomozna(n)
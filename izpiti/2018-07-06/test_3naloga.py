from functools import lru_cache

def simetricen(niz):
    return niz == niz[::-1]

@lru_cache(maxsize=None)
def stevilo_delov (w, je_simetricen):
    if w == '':
        return 0
    if je_simetricen(w):
        return 1
    options = [stevilo_delov(w[:i], je_simetricen) + 
               stevilo_delov(w[i:], je_simetricen) for i in range(1, len(w))]
    n = min(options)
    return n

@lru_cache(maxsize=None)
def razdeli(w, je_simetricen):
    if len(w) == 0:
        return (0, [w])
    if je_simetricen(w):
        return (1, [w])
    options = None
    for i in range(1, len(w)):
        nl, wl = razdeli(w[:i], je_simetricen)
        nr, wr = razdeli(w[i:], je_simetricen)
        k, ws = nl + nr, wl + wr

        if options == None:
            options = (k, ws)
        
        else:
            m, l = options
            if k < m :
                options = (k, ws)
    return options

def vsotno_simetricen(niz):
    if len(niz) == 1:
        return True
    l = [int(c) for c in niz]
    n = int (len(niz) / 2)
    return sum(l[:n]) == sum(l[n:])

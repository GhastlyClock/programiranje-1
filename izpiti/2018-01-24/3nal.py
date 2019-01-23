from functools import lru_cache

test_matrix = [[1, 2, 0, 5], [0, 4, 1, 1], [8, 0, 4, 2]]

test = [[2,4,1,1], [3,2,0,5], [8,0,7,2]]

def maks_jabolk(polje, koraki):

    @lru_cache(maxsize=None)
    def skoki (v, s, k):
        vrednost = polje[v][s]
        if k == 0:
            return 0
        if v == len(polje) - 1:
            if s == len(polje[0]) -1:
                return vrednost
            else:
                return vrednost + skoki(v, s+1, k-1)
        else:
            if s == len(polje[0]) - 1:
                return vrednost + max(skoki(v+1, s, k-1), skoki(v+1, s-1, k-1))
            else:
                return vrednost + max(skoki(v+1, s, k-1), skoki(v, s+1, k-1), skoki(v+1, s-1, k-1))
    return skoki(0, 0, koraki)

print(maks_jabolk(test, 6))


test_matrix = [[1, 2, 0, 5], [0, 4, 1, 1], [8, 0, 4, 2]]
test = [[2,4,1,1], [3,2,0,5], [8,0,7,2]]
def max_points(matrix, max_steps):

    @lru_cache(maxsize=None)
    def jumper(r, c, k):
        val = matrix[r][c]
        #No more steps
        if (k == 0):
            return 0
        #Hit boundaries
        elif (r == len(matrix) - 1):
            #Can't go down
            if (c == len(matrix[r]) - 1):
                #Can't go right
                return val
            else:
                #Can go right
                return val + jumper(r, c+1, k-1)
        else:
            #Can go down
            if (c == len(matrix[r]) - 1):
                #Can't go right
                return val + jumper(r+1, 0, k-1)
            else:
                #Can go right
                return val + max(jumper(r, c+1, k-1), jumper(r+1, 0, k-1))

    #Call function
    return jumper(0,0,max_steps)

print(max_points(test, 6))
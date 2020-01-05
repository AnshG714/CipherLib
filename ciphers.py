import string
import math
import random


def atbash(plaintext):
    res = ""
    for i in range(len(plaintext)):
        if plaintext[i] in string.punctuation or plaintext[i] == ' ':
            res += plaintext[i]
            continue
        charInt = ord(plaintext[i].lower())
        pos = charInt - ord('a')
        if plaintext[i].islower():
            res += chr(ord('z') - pos)
        else:
            res += chr(ord('Z') - pos)

    return res

def decryptAtbash(ciphertext):
    return atbash(ciphertext)

################################################################################
def caesar(plaintext, key):
    res = ""
    for i in range(len(plaintext)):
        if plaintext[i] in string.punctuation or plaintext[i] == ' ':
            res += plaintext[i]
            continue
        charInt = ord(plaintext[i].lower())
        pos = charInt - ord('a')
        if plaintext[i].islower():
            res += chr((pos + key)%26 + ord('a'))
        else:
            res += chr((pos + key)%26 + ord('a')).upper()
    return res

def decryptCaesar(ciphertext, key):
    return caesar(ciphertext, -key)

################################################################################
def findCoPrimes(m):
    res = []
    i = 1
    while i < m:
        if math.gcd(m, i) == 1:
            res.append(i)
        i+=1
    return res

def affine(plaintext, a, b):
    poss_a = findCoPrimes(26)
    assert a in poss_a
    assert 0 <= b <= 26

    res = ''
    for c in plaintext:
        if c in string.punctuation or c == ' ':
            res += c
            continue
        charInt = ord(c.lower()) - ord('a')
        if c.isupper():
            res += chr((charInt * a + b) % 26 + ord('a')).upper()
        else:
            res += chr((charInt * a + b) % 26 + ord('a'))
    return res

def decryptAffine(ciphertext, a, b):
    a_inv = None
    for i in range(1, 26):
        if a*i % 26 == 1:
            a_inv = i
            break
    res = ''

    for c in ciphertext:
        charInt = ord(c.lower()) - ord('a')
        if c in string.punctuation or c == ' ':
            res += c
            continue

        if c.isupper():
            res += chr(a_inv*(charInt - b)%26 + ord('a')).upper()
        else:
            res += chr(a_inv*(charInt - b)%26 + ord('a'))

    return res

################################################################################

def railFence(plaintext, numRows):
    if numRows == 1 or numRows > len(plaintext):
        return plaintext

    L = [""]*numRows
    index = 0
    step = 1

    for s in plaintext:
        L[index] += s
        if index == 0:
            step = 1
        if index == numRows - 1:
            step = -1

        index += step

    return "".join(L)

def decryptRailFence(ciphertext, numRows):
    table = []
    for _ in range(numRows):
        table.append(["\t"]*len(ciphertext))

    rowTracker = 0
    colTracker = 0
    down = True
    i = 0

    if numRows == 1:
        return ciphertext


    #marking the zig-zag pattern
    while (i < len(ciphertext)):
        table[rowTracker][colTracker] = "\n"

        if down:
            rowTracker+=1
        else:
            rowTracker -= 1
            colTracker += 1

        if rowTracker == numRows - 1:
            down = False
        if rowTracker == 0:
            down = True

        i+= 1

    #filling the zigzag row by row
    n = 0
    for i in range(numRows):
        for j in range(len(ciphertext)):
            if table[i][j] == "\n":
                table[i][j] = ciphertext[n]
                n+=1

    #getting result
    res = ""
    rowTracker = 0
    colTracker = 0
    down = True
    i = 0

    while (i < len(ciphertext)):
        res += table[rowTracker][colTracker]

        if down:
            rowTracker+=1
        else:
            rowTracker -= 1
            colTracker += 1

        if rowTracker == numRows - 1:
            down = False
        if rowTracker == 0:
            down = True

        i+= 1

    return res

################################################################################
def generateRandomKey():
    #for actual software
    s = 'abcdefghiklmnopqrstuvwxyz'
    l = list(s)
    random.shuffle(l)
    return ''.join(l)

def polybiusSquare(plaintext, key):
    conversionDic = {}
    for index in range(len(key)):
        cipherCode = ""
        cipherCode += chr(ord('A') + index//5)
        cipherCode += chr(ord('A') + index%5)
        conversionDic[key[index]] = cipherCode

    res = ""
    for s in plaintext:
        res += conversionDic[s]
    return res

def decryptPolybiusSquare(ciphertext, key):
    index = 1
    res = ""
    while index < len(ciphertext):
        i = (ord(ciphertext[index-1]) - ord('A'))*5 + ord(ciphertext[index]) - ord('A')
        res += key[i]
        index += 2
    return res

################################################################################

def simpleSubstitution(plaintext, key):
    conversionDic = {}
    i = ord('a')
    while i <= ord('z'):
        conversionDic[chr(i)] = key[i - ord('a')]
        i += 1

    res = ""
    for s in plaintext:
        if s not in string.punctuation and s != " ":
            res += conversionDic[s]
        else:
            res += s

    return res

def decryptSimpleSubstitution(ciphertext, key):
    conversionDic = {}
    i = ord('a')
    while i <= ord('z'):
        conversionDic[key[i - ord('a')]] = chr(i)
        i += 1

    res = ""
    for s in ciphertext:
        if s not in string.punctuation and s != " ":
            res += conversionDic[s]
        else:
            res += s

    return res

################################################################################

def columnarTransposition(plaintext, key):

    plaintext += "_"*(len(key) - (len(plaintext))%len(key))
    print(plaintext)
    print(len(plaintext))

    arr = [""]*len(key)
    i = 0
    while i < len(plaintext):
        if plaintext[i] not in string.punctuation and plaintext[i] != " ":
            arr[i%len(key)] += plaintext[i]
        i += 1

    keyArr = []
    for c in key:
        keyArr.append(c)

    Z = [x for _,x in sorted(zip(keyArr, arr))]

    return ''.join(Z)

def decryptColumnarTransposition(ciphertext, key):
    numRows = int(math.ceil(len(ciphertext)/len(key)))

    relArr = []
    for i in range(len(key)):
        relArr.append(i)

    keyArr = []
    for c in key:
        keyArr.append(c)

    Z = [x for _,x in sorted(zip(keyArr, relArr))]

    matrix = [Z]
    for _ in range(numRows):
        matrix.append([None]*len(key))

    numNulls = numRows*len(key) - len(ciphertext)
    nullCount = 0
    while nullCount < numNulls:
        matrix[-1][len(key)-nullCount-1] = "_"
        nullCount += 1

    ciphertextindex = 0
    colIndex = 0
    rowIndex = 1
    print(matrix)
    print(Z)
    while ciphertextindex < len(ciphertext):
        if rowIndex <= numRows and matrix[rowIndex][Z[colIndex]] != '_':
            matrix[rowIndex][Z[colIndex]] = ciphertext[ciphertextindex]
            ciphertextindex += 1
            rowIndex+=1
        else:
            rowIndex = 1
            colIndex += 1

    res = ""
    for row in matrix[1:]:
        for char in row:
            if char != "_":
                res += char
    return res

################################################################################

def autokey(plaintext, key):
    plaintext = plaintext.upper()
    key = key.upper()
    keyStream = key + plaintext[:len(plaintext) - len(key)]

    res = ""
    print(plaintext)
    for i in range(len(plaintext)):
        res += chr((ord(plaintext[i]) + ord(keyStream[i]) - 2*ord('A'))%26 + ord('A'))
    return res

def decryptAutokey(ciphertext, key):
    pass

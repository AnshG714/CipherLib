import string
import math
import random
import numpy as np
from exceptions import *

def generateRandomKey():
    #for actual software
    s = 'abcdefghiklmnopqrstuvwxyz'
    l = list(s)
    random.shuffle(l)
    return ''.join(l)

def transform(plaintext):
    """
    This function returns a string which transforms the plaintext as follows:
    1. Removes all the whitespaces and punctuation
    2. Makes all letters uppercase

    plaintext: The text to transform
    Precondition: text is a string

    """
    res = ""
    for c in plaintext:
        if c not in string.punctuation and c != " ":
            res += c.upper()
    return res

################################################################################

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
    for i in range(len(plaintext)):
        res += chr((ord(plaintext[i]) + ord(keyStream[i]) - 2*ord('A'))%26 + ord('A'))
    return res

def decryptAutokey(ciphertext, key):
    ciphertext = ciphertext.upper()
    key = key.upper()

    res = ""
    keyStream = key

    for i in range(len(ciphertext)):
        if ord(ciphertext[i]) < ord(keyStream[i]):
            c = ord('Z') - ord(keyStream[i]) + ord(ciphertext[i]) - ord('A')
            res += chr(c + ord('A')+1)
            keyStream += chr(c + ord('A')+1)
        else:
            res += chr(ord(ciphertext[i]) - ord(keyStream[i]) + ord('A'))
            keyStream += chr(ord(ciphertext[i]) - ord(keyStream[i]) + ord('A'))
    return res

################################################################################

def beufort(plaintext, key):
    plaintext = plaintext.upper()
    key = key.upper()
    keyStream = key*(len(plaintext)//len(key)) + key[:len(plaintext)%len(key)]

    res = ""
    for i in range(len(plaintext)):
        if ord(plaintext[i]) > ord(keyStream[i]):
            c = ord('Z') - ord(plaintext[i]) + ord(keyStream[i]) - ord('A')
            res += chr(c + ord('A')+1)
        else:
            res += chr(ord(keyStream[i]) - ord(plaintext[i]) + ord('A'))
    return res

def decryptBeufort(ciphertext, key):
    return beufort(ciphertext, key)

################################################################################
def porta(plaintext, key):
    plaintext = plaintext.upper()
    key = key.upper()
    matrix = [['N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'A', 'B',
     'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M'], ['O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'N', 'M', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L'], ['P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'N', 'O', 'L', 'M', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K'], ['Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'N', 'O', 'P', 'K', 'L', 'M', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'], ['R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'N', 'O', 'P', 'Q', 'J', 'K', 'L', 'M', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'], ['S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'N', 'O', 'P', 'Q', 'R', 'I', 'J', 'K', 'L', 'M', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'], ['T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'N', 'O', 'P', 'Q', 'R', 'S', 'H', 'I', 'J', 'K', 'L', 'M', 'A', 'B', 'C', 'D', 'E', 'F', 'G'], ['U', 'V', 'W', 'X', 'Y', 'Z', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'A', 'B', 'C', 'D', 'E', 'F'], ['V', 'W', 'X', 'Y', 'Z', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'A', 'B', 'C', 'D', 'E'], ['W', 'X', 'Y', 'Z', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'A', 'B', 'C', 'D'], ['X', 'Y', 'Z', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'A', 'B', 'C'], ['Y', 'Z', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'A', 'B'], ['Z', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'A']]

    keyStream = key*(len(plaintext)//len(key)) + key[:len(plaintext)%len(key)]

    res = ""
    for i in range(len(plaintext)):
        res += matrix[(ord(keyStream[i]) - ord('A'))//2][ord(plaintext[i]) - ord('A')]
    return res

def decryptPorta(ciphertext, key):
    ciphertext = ciphertext.upper()
    key = key.upper()

    return porta(ciphertext, key)

################################################################################

def runningKey(plaintext, key):
    #use long key, i.e., len(key) >= len(plaintext)

    plaintext = plaintext.upper()
    key = key.upper()

    #getting key in correct form
    keyStream = ""
    i = 0
    keyIndex = 0
    while i < len(plaintext):
        if key[keyIndex].isalpha():
            keyStream += key[keyIndex]
            i += 1
        keyIndex += 1

    return autokey(plaintext, keyStream)

def decryptRunningKey(ciphertext, key):
    ciphertext = ciphertext.upper()
    key = key.upper()

    #getting key in correct form
    keyStream = ""
    i = 0
    keyIndex = 0
    while i < len(ciphertext):
        if key[keyIndex].isalpha():
            keyStream += key[keyIndex]
            i += 1
        keyIndex += 1

    res = ""

    for i in range(len(ciphertext)):
        if ord(ciphertext[i]) < ord(key[i]):
            c = ord('Z') - ord(key[i]) + ord(ciphertext[i]) - ord('A')
            res += chr(c + ord('A')+1)
        else:
            res += chr(ord(ciphertext[i]) - ord(key[i]) + ord('A'))
    return res

################################################################################

def vigenere(plaintext, key):
    plaintext = plaintext.upper()
    key = key.upper()

    keyStream = key*(len(plaintext)//len(key)) + key[:len(plaintext)%len(key)]
    res = ""
    for i in range(len(plaintext)):
        res += chr((ord(plaintext[i]) + ord(keyStream[i]) - 2*ord('A'))%26 + ord('A'))
    return res

def decryptVigenere(ciphertext, key):
    ciphertext = ciphertext.upper()
    key = key.upper()

    keyStream = key*(len(ciphertext)//len(key)) + key[:len(ciphertext)%len(key)]

    res = ""
    for i in range(len(ciphertext)):
        if ord(ciphertext[i]) < ord(keyStream[i]):
            c = ord('Z') - ord(keyStream[i]) + ord(ciphertext[i]) - ord('A')
            res += chr(c + ord('A')+1)
        else:
            res += chr(ord(ciphertext[i]) - ord(keyStream[i]) + ord('A'))
    return res

################################################################################

def fourSquare(plaintext, key1, key2):
    plaintext = plaintext.upper()
    if len(plaintext)%2 == 1:
        plaintext += "X"

    temp = ""
    for c in plaintext:
        if c == 'J':
            temp += 'I'
        else:
            temp += c

    plaintext = temp

    res = ""
    i = 1
    while i < len(plaintext):
        letter1 = plaintext[i-1]
        letter2 = plaintext[i]

        if ord(letter1) > ord('J'):
            letter1row = (ord(letter1) - ord('A') - 1)//5
            letter1col = (ord(letter1) - ord('A') - 1)%5
        else:
            letter1row = (ord(letter1) - ord('A'))//5
            letter1col = (ord(letter1) - ord('A'))%5

        if ord(letter2) > ord('J'):
            letter2row = (ord(letter2) - ord('A') - 1)//5
            letter2col = (ord(letter2) - ord('A') - 1)%5
        else:
            letter2row = (ord(letter2) - ord('A'))//5
            letter2col = (ord(letter2) - ord('A'))%5


        res += key1[5*letter1row + letter2col]
        res += key2[5*letter2row + letter1col]
        i+= 2

    return res

def decryptFourSquare(ciphertext, key1, key2):
    alpha = 'ABCDEFGHIKLMNOPQRSTUVWXYZ'
    res = ""
    i = 1
    while i < len(ciphertext):
        letter1 = ciphertext[i-1]
        letter2 = ciphertext[i]

        l1index = key1.index(letter1)
        l2index = key2.index(letter2)

        letter1row = l1index//5
        letter1col = l1index%5
        letter2row = l2index//5
        letter2col = l2index%5

        res += alpha[5*letter1row + letter2col]
        res += alpha[5*letter2row + letter1col]
        i+= 2

    return res
################################################################################

def hill(plaintext, matrix):
    n = len(matrix)
    plaintext = transform(plaintext)
    for element in matrix:
        assert type(element) == list
        assert len(element) == n

    mat = np.array(matrix)
    if len(plaintext) % n != 0:
        plaintext += 'X'*(n - len(plaintext)%n)

    res = ""
    i = 0
    while i < len(plaintext):
        substr = plaintext[i:i+n]
        res += hillHelper(mat, substr)
        i += n

    return res

def decryptHill(ciphertext, matrix):
    mat = np.array(matrix)
    n = len(matrix)
    d = int(round(np.linalg.det(mat), 0)) % 26
    d_inv = _findDInv(d)
    invMat = d_inv * np.array(getConjugate(mat)).transpose()

    i = 0
    res = ""
    while i < len(ciphertext):
        substr = ciphertext[i:i+n]
        res += hillHelper(invMat, substr)
        i += n
    return res

def _findDInv(d):
    for i in range(1, 26):
        if i*d % 26 == 1:
            return i

    raise InvalidMatrixException('Invalid matrix - cannot find determinant inverse satisying invariants')

def hillHelper(mat, substr):
    res = ""
    col = []
    for c in substr:
        col.append(ord(c) - ord('A'))
    a = np.array(col)
    vector = mat.dot(a) % 26
    for k in range(len(vector)):
        res += chr(vector[k] + ord('A'))
    return res

def getConjugate(matrix):
    c = []
    for i in range(len(matrix)):
        c.append([None]*len(matrix))

    for i in range(len(matrix)):
        for j in range(len(matrix)):
            omitIJMat = np.delete(np.delete(matrix, i, 0), j, 1)
            c[i][j] = int(round(((-1)**(i+j))*np.linalg.det(omitIJMat), 0))
    return c

def formKeySquare(key):
    #remove duplicate letters
    usedLetters = set()
    keySquare = ""
    for c in key:
        if c not in usedLetters:
            keySquare += c
            usedLetters.add(c)

    alpha = 'ABCDEFGHIKLMNOPQRSTUVWXYZ'

    for c in alpha:
        if c not in usedLetters:
            keySquare += c
            usedLetters.add(c)

    return keySquare

def playfair(plaintext, key):
    if len(plaintext) == 0:
        return ""

    plaintext = transform(plaintext)
    keySquare = formKeySquare(key)
    plaintextCopy = ""

    if len(plaintext) % 2 == 1:
        plaintext += 'X'

    i = 0
    while i < len(plaintext)-1:
        if plaintext[i] == plaintext[i+1]:
            plaintextCopy += plaintext[i] + 'X'
        else:
            plaintextCopy += plaintext[i] + plaintext[i+1]
        i += 2

    res = ""

    for i in range(1, len(plaintextCopy), 2):
        pair = plaintextCopy[i-1:i+1]
        index1 = keySquare.index(pair[0])
        letter1row = index1//5
        letter1col = index1%5
        index2 = keySquare.index(pair[1])
        letter2row = index2//5
        letter2col = index2%5

        if letter1col != letter2col and letter1row != letter2row:
            res += keySquare[5*letter1row + letter2col] + keySquare[5*letter2row + letter1col]
        elif letter1row == letter2row:
            res += keySquare[5*letter1row + (letter1col + 1)%5] + keySquare[5*letter1row + (letter2col + 1)%5]
        else:
            res += keySquare[5*((letter1row+1)%5) + letter1col] + keySquare[5*((letter2row+1)%5) + letter1col]

    return res

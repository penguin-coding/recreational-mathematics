# Python 3 program written to solve question 40 of project Euler

def write_word(q):
    # purpose : writes the q first characters of the infinite word described by
    #           get_digit.
    # input   : q, a positive integer
    # output  : a string, containing the appropriate characters (which are all
    #           digits)
    # note    : there are no input checks since this code needs to be written 
    #           quickly and is for personal use, hence robustness is not a large
    #           concern of mine. 

    word = ''
    for i in range(1,q+1): word += str(i)
    return(word)

def get_length_word(q):
    # purpose : calculates the number of characters in the infinite word written
    #           with the numbers 1 through q
    # input   : q, a positive integer
    # output  : a positive integer

    length = 0

    while(q>=1):
        order = len(str(q))
        # The number of numbers in the current order:       
        value_in_order = q - (10**(order-1) - 1)
        length += value_in_order*order
        q -= value_in_order

    return(length)

def get_digit(n):
    # purpose : produces the nth digit in the number created by writting 1, 
    #           followed by 2, 3, 4, ..., 10, 11, etc so that the start of the 
    #           number is 123456789101112131415161718192021222324...
    #           This number is of course infinite. It is traditionally
    #           considered as an irrational number 0.123456... (which is the
    #           form in which it is described in the project Euler question)
    #
    # input   : n - a positive integer
    #
    # output  : a number from 0 to 9, the nth digit of the above described 
    #           infinite number
    #
    # notes   : since for many of the required calculations, it is easier to use
    #           strings as opposed to integers or floats, the infinite number
    #           is often referred to as a 'word'
    #
    # We calculate the answer in 3 steps:
    # 1. find q such that q is the smallest number with which we can write the 
    #    first n letters of the infinite word
    # 2. find the number of characters in the word produced by using the numbers
    #    1 to q-1
    # 3. define 'a' to be the difference between n and the result of 2. The 
    #    answer is the a th digit of the number q

    # 1. :
    #   1.1: start with a large guess for q
    #   1.2: check the current guess produces a word of length n or more, if it
    #        doesn't, q + 1 is the correct answer. 
    #   1.3: use an updating formula on q, such that the new guess NEVER
    #        under-estimates q

    # 1.1
    q = n  # a gross over-estimate, which is fine

    while (get_length_word(q-1)>=n): # 1.2

        delta = get_length_word(q)-n
        # 1.3:
        order = len(str(q))        # assume removing each number removes the max
        removal = int(delta/order) # number of characters

        if removal<1: removal = 1  # but we must also remove at least one number
        
        q -= removal
    
    # 2.
    q_m1_length = get_length_word(q-1)

    # 3.
    a = n-q_m1_length
    return(int(str(q)[a-1]))


answer = 1
for i in range(1,7):
  answer *= get_digit(10**i)

print(answer)

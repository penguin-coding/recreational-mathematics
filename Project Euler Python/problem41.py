# Python 3 file attempting to solve Project Euler question 41

no_repeats = [i for i in range(1,1400) if len("".join(set(str(i))))==len(str(i))]

answers = []

for a in no_repeats:
    for b in no_repeats:
        ab = ''.join(set(str(a))) + ''.join(set(str(b)))  # check a and b 
        if len(ab)!=len(str(a))  + len(str(b)): continue

        string = str(a) + str(b) + str(a*b)
        for i in range(1,10):
            if str(i) not in string: break
        else: answers.append(a*b)

answers_as_set = []

for i in answers:
    if i not in answers_as_set: answers_as_set.append(i)

print(len(answers_as_set))

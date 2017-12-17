import random

from problog.logic import Constant,Term

def getEvidences(board_file, board_samples, sample_indexes):
    evidenceStrings = []
    with open(board_file, 'r') as fp:
        total = int(fp.readline().strip())
        if (board_samples > total):
            board_samples = total
            print('There are more samples than board configurations. New sample size is: ' + str(board_samples))
        if(not sample_indexes):
            sample_indexes = random.sample(range(0, total), board_samples)
        for i, line in enumerate(fp):
            if i in sample_indexes:
                evidenceStrings.append(line.strip())

    evidences = []
    for evidenceString in evidenceStrings:
        evidenceString = evidenceString[2:(len(evidenceString) - 2)]
        data = evidenceString.split('], [')
        evidence = []
        for i, row in enumerate(data):
            rowSplit = row.split(', ')
            for j, block in enumerate(rowSplit):
                evidence.append((Term('block', Term(block), Constant(j), Constant((i))), True))
        evidences.append(evidence)

    return evidences
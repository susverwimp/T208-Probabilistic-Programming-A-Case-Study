from __future__ import print_function
import sys
import time
import os
import random

from problog.program import PrologString
from problog.engine import DefaultEngine
from problog.formula import LogicDAG
from problog.ddnnf_formula import DDNNF
from problog.cnf_formula import CNF
from problog.logic import Constant,Term
from problog.tasks import sample

def main(width, height, turns, board_samples, uniform, samples):
    with open("files/model_problog.pl") as prologfile:
        logic_program = prologfile.read()

    if(uniform):
        strategies = ['uniform', 'color_ratio', 'possible_score', 'possible_score_improved']
    else:
        strategies = ['color_ratio', 'possible_score', 'possible_score_improved']

    logic_program += '\nwidth(' + str(width - 1) + ').\n'
    logic_program += 'height(' + str(height - 1) + ').\n'
    prepareDB(logic_program, width, height, board_samples, strategies, turns,samples)



def prepareDB(logic_program, width, height, board_samples, strategies, turns, samples):
    boardconfigurationsFileName = "files/" + str(width) + 'x' + str(height) + '.txt'

    evidenceStrings = []
    with open(boardconfigurationsFileName, 'r') as fp:
        total = int(fp.readline().strip())
        if (board_samples > total):
            board_samples = total
            print('There are more samples than board configurations. New sample size is: ' + str(board_samples))
        sampleIndexes = random.sample(range(0, total), board_samples)
        for i, line in enumerate(fp):
            if i in sampleIndexes:
                evidenceStrings.append(line.strip())

    evidences = []
    for evidenceString in evidenceStrings:
        evidenceString = evidenceString[2:(len(evidenceString) - 2)]
        data = evidenceString.split('], [')
        evidence = []
        for i, row in enumerate(data):
            rowSplit = row.split(', ')
            for j, block in enumerate(rowSplit):
                evidence.append((Term('block', Term(block), Constant(j), Constant((height - 1 - i))), True))
        evidences.append(evidence)

    p = PrologString(logic_program)

    if(samples == -1):
        engine = DefaultEngine(label_all=True)
        start_time = time.time()
        db = engine.prepare(p)
        print('%s: %.4fs' % ('parsing initial', time.time() - start_time))
        print(db)

    import itertools
    s = [strategies, [evidences]]
    permutations = list(itertools.product(*s))
    for strategy in strategies:
        if (samples == -1):
            # add strategy fact to the program
            dbPerm = db.extend()
            dbPerm += Term('strategy', Term(strategy))
            perm_string = 'turn:' + str(turns) + ' strategy:' + strategy + ' size:' + str(width) + 'x' + str(height) + ' '
            problogChain(engine, dbPerm, evidences, turns, perm_string)
        else:
            sampleModel(logic_program, samples, turns, strategy, evidences)

def sampleModel(logic_program, samples, turns, strategy, evidences):
    logic_program_copy = logic_program + "strategy(" + strategy + ").\n"
    logic_program_copy += str(Term('query', Term('score_of_turn', Constant(turns), None))) + '.\n'
    print(strategy)
    for evidence in evidences:
        logic_program_copy2 = logic_program_copy
        print('evidence: ' + str(evidence))
        for evidenceTerm in evidence:
            logic_program_copy2 += "evidence" + str(evidenceTerm).lower() + ".\n"

        print(logic_program_copy2)
        model = PrologString(logic_program_copy2)
        start_time = time.time()
        result = sample.sample(model, n=samples, propagate_evidence=True)
        elapsed_time = time.time() - start_time
        print('%s: %.4fs' % ('samples', elapsed_time))
        for i,r in enumerate(result):
            print(str(i) + r)


def problogChain(engine, dbPerm, evidences, turns, perm_string):
    query = Term('score_of_turn', Constant(turns), None)

    average_ground_time = 0
    average_acyclic_time = 0
    average_cnf_time = 0
    average_compile_time = 0
    average_evaluate_time = 0
    average_total_time = 0

    average_score_dict = {}

    for evidence in evidences:
        print('evidence: ' + str(evidence))
        # print('\n=== Ground Program ===')
        total_time = 0
        start_time = time.time()
        gp = engine.ground_all(dbPerm, queries=[query], evidence=evidence, propagate_evidence=True)
        elapsed_time = time.time() - start_time
        total_time += elapsed_time
        average_ground_time += elapsed_time
        print('%s: %.4fs' % (perm_string + 'ground', elapsed_time))

        # print('\n=== Acyclic Ground Program ===')
        start_time = time.time()
        dag = LogicDAG.create_from(gp)
        elapsed_time = time.time() - start_time
        total_time += elapsed_time
        average_acyclic_time += elapsed_time
        print('%s: %.4fs' % (perm_string + 'acyclic', elapsed_time))

        # print('\n=== Conversion to CNF ===')
        start_time = time.time()
        cnf = CNF.createFrom(dag)
        elapsed_time = time.time() - start_time
        total_time += elapsed_time
        average_cnf_time += elapsed_time
        print('%s: %.4fs' % (perm_string + 'convert to CNF', elapsed_time))

        # print('\n=== Compile to d-DNNF ===')
        start_time = time.time()
        nnf = DDNNF.createFrom(cnf)
        elapsed_time = time.time() - start_time
        total_time += elapsed_time
        average_compile_time += elapsed_time
        print('%s: %.4fs' % (perm_string + 'compile', elapsed_time))

        # print('\n=== Evaluation result ===')
        start_time = time.time()
        result = nnf.evaluate()
        elapsed_time = time.time() - start_time
        total_time += elapsed_time
        average_evaluate_time += elapsed_time
        average_total_time += total_time
        print('%s: %.4fs' % (perm_string + 'evaluate', elapsed_time))
        print('%s: %.4fs' % (perm_string + 'total time', total_time))
        for it in result.items():
            key = str(it[0])[16:17]
            value = it[1]
            if key in average_score_dict:
                average_score_dict[key] += value
            else:
                average_score_dict[key] = value
            print('%s : %s' % (it))

    evidence_samples = len(evidences)
    average_ground_time /= evidence_samples
    average_acyclic_time /= evidence_samples
    average_cnf_time /= evidence_samples
    average_compile_time /= evidence_samples
    average_evaluate_time /= evidence_samples
    average_total_time /= evidence_samples

    print()
    for keys, values in average_score_dict.items():
        values /= evidence_samples
        print('%s: %.12f' % ('average ' + perm_string + 'score of ' + keys, values))
    print()
    print('%s: %.4fs' % ('average ' + perm_string + 'ground', average_ground_time))
    print('%s: %.4fs' % ('average ' + perm_string + 'acyclic', average_acyclic_time))
    print('%s: %.4fs' % ('average ' + perm_string + 'convert to CNF', average_cnf_time))
    print('%s: %.4fs' % ('average ' + perm_string + 'compile', average_compile_time))
    print('%s: %.4fs' % ('average ' + perm_string + 'evaluate', average_evaluate_time))
    print('%s: %.4fs' % ('average ' + perm_string + 'total', average_total_time))
    print()

if __name__ == '__main__':
    width = 3
    height = 3
    turns = 1
    boardconfiguration_samples = 1
    uniform_included = False
    samples = -1
    main(width,height,turns,boardconfiguration_samples,uniform_included, samples)
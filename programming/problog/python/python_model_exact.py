from __future__ import print_function
import time

from problog.program import PrologString
from problog.engine import DefaultEngine
from problog.formula import LogicDAG
from problog.ddnnf_formula import DDNNF
from problog.cnf_formula import CNF
from problog.logic import Constant,Term

from files.evidence import getEvidences

def main(width, height, turns, board_samples, uniform_included):
    with open("files/model_problog.pl") as prologfile:
        logic_program = prologfile.read()
    logic_program += '\nwidth(' + str(width - 1) + ').\n'
    logic_program += 'height(' + str(height - 1) + ').\n'

    p = PrologString(logic_program)

    engine = DefaultEngine(label_all=True)
    start_time = time.time()
    db = engine.prepare(p)
    print('%s: %.4fs' % ('parsing initial', time.time() - start_time))

    if (uniform_included):
        strategies = ['uniform', 'color_ratio', 'possible_score', 'possible_score_improved']
    else:
        strategies = ['color_ratio', 'possible_score', 'possible_score_improved']

    query = Term('score_of_turn', Constant(turns), None)
    evidences = getEvidences("files/" + str(width) + "x" + str(height) + ".txt", board_samples)

    for strategy in strategies:
        dbPerm = db.extend()
        dbPerm += Term('strategy', Term(strategy))
        perm_string = 'turn:' + str(turns) + ' strategy:' + strategy + ' size:' + str(width) + 'x' + str(height) + ' '

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
            for it in result.items():
                key = str(it[0])[16:17]
                value = it[1]
                if key in average_score_dict:
                    average_score_dict[key] += value
                else:
                    average_score_dict[key] = value
                print('%s : %s' % (it))
            print('%s: %.4fs' % (perm_string + 'total time', total_time))

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
        print('--------------------------------')
        print('--------------------------------')




if __name__ == '__main__':
    width = 3
    height = 3
    turns = 2
    boardconfiguration_samples = 5
    uniform_included = False
    main(width, height, turns, boardconfiguration_samples, uniform_included)
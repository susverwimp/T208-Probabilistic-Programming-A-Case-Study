from __future__ import print_function
import time

from problog.program import PrologString
from problog.engine import DefaultEngine
from problog.formula import LogicDAG
from problog.ddnnf_formula import DDNNF
from problog.cnf_formula import CNF
from problog.logic import Constant,Term,list2term
from board import printBoard

from files.evidence import getEvidences

def main(width, height, turns, board_samples):
    with open("files/model_problog_meta.pl") as prologfile:
        logic_program = prologfile.read()
    logic_program += '\nwidth(' + str(width - 1) + ').\n'
    logic_program += 'height(' + str(height - 1) + ').\n'
    logic_program += 'strategy(uniform).\n'
    p = PrologString(logic_program)

    engine = DefaultEngine(label_all=True)
    start_time = time.time()
    db = engine.prepare(p)
    print('%s: %.4fs' % ('parsing initial', time.time() - start_time))

    evidences = getEvidences("files/" + str(width) + "x" + str(height) + ".txt", board_samples, [65411])

    perm_string = 'turn:' + str(turns) + ' strategy:uniform size:' + str(width) + 'x' + str(height) + ' '

    total_2_turns = turns / 2
    total_1_turns = turns % 2

    for evidence in evidences:
        average_ground_time = 0
        average_acyclic_time = 0
        average_cnf_time = 0
        average_compile_time = 0
        average_evaluate_time = 0
        average_total_time = 0

        evidence_copy = evidence

        positions = []

        for i in range(0,turns - 1):
            printBoard(width, height, evidence_copy)
            perm_string = 'turn:1 strategy:uniform size:' + str(width) + 'x' + str(height) + ' '

            query = Term('score_of_turn', Constant(3), None,
                         list2term([list2term([Constant(0), Constant(0)]), list2term([Constant(0), Constant(1)]), list2term([Constant(0), Constant(1)])]))
            # query = Term('board', Constant(1), None, None, None)
            # query = Term('score_of_turn', Constant(1), None, None)
            # query = Term('score_of_initial_board_with_position', None, None)


            total_time = 0
            start_time = time.time()
            gp = engine.ground_all(db, queries=[query], evidence=evidence_copy, propagate_evidence=True)
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
            print('%s: %.4fs' % (perm_string + 'evaluate', elapsed_time))
            for it in result.items():
                print('%s : %s' % (it))
            print('%s: %.4fs' % (perm_string + 'total time', total_time))




if __name__ == '__main__':
    width = 4
    height = 4
    turns = 3
    boardconfiguration_samples = 1
    main(width, height, turns, boardconfiguration_samples)
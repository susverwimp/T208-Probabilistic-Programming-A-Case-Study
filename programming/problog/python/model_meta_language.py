from __future__ import print_function
import time

from problog.program import PrologString
from problog.engine import DefaultEngine
from problog.formula import LogicDAG
from problog.ddnnf_formula import DDNNF
from problog.cnf_formula import CNF
from problog.logic import Constant,Term,list2term,term2list
from board import printBoard

import operator

from files.evidence import getEvidences, changeBoardToEvidence

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

    evidences = getEvidences("files/" + str(width) + "x" + str(height) + ".txt", board_samples, [])

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

        evidences_copy = [evidence]

        positions = []

        for i in range(0,turns):
            map_weighted_score = {}
            map_position_board = {}
            for evidence_copy in evidences_copy:
                printBoard(width, height, evidence_copy)
                perm_string = 'turn:1 strategy:uniform size:' + str(width) + 'x' + str(height) + ' '

                # query = Term('score_of_turn', Constant(3), None,
                #              list2term([list2term([Constant(0), Constant(0)]), list2term([Constant(0), Constant(1)]),
                #                         list2term([Constant(0), Constant(1)])]))
                # query = Term('score_of_turn', Constant(1), None, list2term([list2term([Constant(2), Constant(2)])]))
                # query = Term('board', Constant(1), None, None, None)
                # query = Term('score_of_turn', Constant(1), None, None)
                # query = Term('score_of_initial_board_with_position', None, None)


                query = Term('board_without_press', Constant(1), None, None, None)

                total_time = 0
                start_time = time.time()
                gp = engine.ground_all(db, queries=[query], evidence=evidence_copy, propagate_evidence=True)
                elapsed_time = time.time() - start_time
                total_time += elapsed_time
                average_total_time += elapsed_time
                average_ground_time += elapsed_time
                print('%s: %.4fs' % (perm_string + 'ground', elapsed_time))

                # print('\n=== Acyclic Ground Program ===')
                start_time = time.time()
                dag = LogicDAG.create_from(gp)
                elapsed_time = time.time() - start_time
                total_time += elapsed_time
                average_total_time += elapsed_time
                average_acyclic_time += elapsed_time
                print('%s: %.4fs' % (perm_string + 'acyclic', elapsed_time))

                # print('\n=== Conversion to CNF ===')
                start_time = time.time()
                cnf = CNF.createFrom(dag)
                elapsed_time = time.time() - start_time
                total_time += elapsed_time
                average_total_time += elapsed_time
                average_cnf_time += elapsed_time
                print('%s: %.4fs' % (perm_string + 'convert to CNF', elapsed_time))

                # print('\n=== Compile to d-DNNF ===')
                start_time = time.time()
                nnf = DDNNF.createFrom(cnf)
                elapsed_time = time.time() - start_time
                total_time += elapsed_time
                average_total_time += elapsed_time
                average_compile_time += elapsed_time
                print('%s: %.4fs' % (perm_string + 'compile', elapsed_time))

                # print('\n=== Evaluation result ===')
                start_time = time.time()
                result = nnf.evaluate()
                elapsed_time = time.time() - start_time
                total_time += elapsed_time
                average_total_time += elapsed_time
                average_evaluate_time += elapsed_time
                print('%s: %.4fs' % (perm_string + 'evaluate', elapsed_time))

                for it in result.items():
                    print('%s : %s' % (it))
                    probability = it[1]
                    board = it[0].args[1]
                    score = it[0].args[2].functor
                    position = it[0].args[3].args[0]
                    if position in map_weighted_score:
                        map_weighted_score[position] += score * probability
                    else:
                        map_weighted_score[position] = score * probability
                    if position in map_position_board:
                        map_position_board[position].append(board)
                    else:
                        map_position_board[position] = [board]
                print('%s: %.4fs' % (perm_string + 'total time', total_time))

            best_weighted_position = max(map_weighted_score.iteritems(), key=operator.itemgetter(1))[0]
            print(best_weighted_position)
            positions.append(best_weighted_position)

            evidences_copy = []
            for board  in map_position_board[best_weighted_position]:
                evidences_copy.append(changeBoardToEvidence(board))

        print(positions)
        query = Term('score_of_turn_without_press', Constant(turns), None, list2term(positions))
        evidences_copy = evidence
        printBoard(3,3,evidences_copy)

        start_time = time.time()
        gp = engine.ground_all(db, queries=[query], evidence=evidences_copy, propagate_evidence=True)
        elapsed_time = time.time() - start_time
        total_time += elapsed_time
        average_total_time += elapsed_time
        average_ground_time += elapsed_time
        print('%s: %.4fs' % (perm_string + 'ground', elapsed_time))

        # print('\n=== Acyclic Ground Program ===')
        start_time = time.time()
        dag = LogicDAG.create_from(gp)
        elapsed_time = time.time() - start_time
        total_time += elapsed_time
        average_total_time += elapsed_time
        average_acyclic_time += elapsed_time
        print('%s: %.4fs' % (perm_string + 'acyclic', elapsed_time))

        # print('\n=== Conversion to CNF ===')
        start_time = time.time()
        cnf = CNF.createFrom(dag)
        elapsed_time = time.time() - start_time
        total_time += elapsed_time
        average_total_time += elapsed_time
        average_cnf_time += elapsed_time
        print('%s: %.4fs' % (perm_string + 'convert to CNF', elapsed_time))

        # print('\n=== Compile to d-DNNF ===')
        start_time = time.time()
        nnf = DDNNF.createFrom(cnf)
        elapsed_time = time.time() - start_time
        total_time += elapsed_time
        average_total_time += elapsed_time
        average_compile_time += elapsed_time
        print('%s: %.4fs' % (perm_string + 'compile', elapsed_time))

        # print('\n=== Evaluation result ===')
        start_time = time.time()
        result = nnf.evaluate()
        elapsed_time = time.time() - start_time
        total_time += elapsed_time
        average_total_time += elapsed_time
        average_evaluate_time += elapsed_time
        print('%s: %.4fs' % (perm_string + 'evaluate', elapsed_time))

        for it in result.items():
            print('%s : %s' % (it))

        print('%s: %.4fs' % ('total time', average_total_time))

if __name__ == '__main__':
    width = 5
    height = 5
    turns = 2
    boardconfiguration_samples = 1
    main(width, height, turns, boardconfiguration_samples)
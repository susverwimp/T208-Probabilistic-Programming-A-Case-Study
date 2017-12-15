from __future__ import print_function
import time

from problog.program import PrologString
from problog import get_evaluatable
from files.evidence import getEvidences

def main(width, height, turns, board_samples, uniform_included):
    with open("files/model_problog.pl") as prologfile:
        logic_program = prologfile.read()
    logic_program += '\nwidth(' + str(width - 1) + ').\n'
    logic_program += 'height(' + str(height - 1) + ').\n'
    logic_program += "query(score_of_turn(" + str(turns) + ",_)).\n"

    if (uniform_included):
        strategies = ['uniform', 'color_ratio', 'possible_score', 'possible_score_improved']
    else:
        strategies = ['color_ratio', 'possible_score', 'possible_score_improved']

    evidences = getEvidences("files/" + str(width) + "x" + str(height) + ".txt", board_samples)

    for strategy in strategies:
        perm_string = 'turn:' + str(turns) + ' strategy:' + strategy + ' size:' + str(width) + 'x' + str(height) + ' '
        logic_program_strategy = logic_program + "strategy(" + strategy + ").\n"
        for evidence in evidences:
            logic_program_evidence = logic_program_strategy
            print('evidence: ' + str(evidence))
            for evidenceTerm in evidence:
                logic_program_evidence += "evidence" + str(evidenceTerm).lower() + ".\n"

            # Parse the Prolog string
            p = PrologString(logic_program_evidence)

            # Compile the Prolog model into
            knowledge = get_evaluatable().create_from(p)
            print(knowledge)

            result = knowledge.evaluate()

    print(result)

if __name__ == '__main__':
    width = 2
    height = 2
    turns = 1
    boardconfiguration_samples = 5
    uniform_included = False
    main(width, height, turns, boardconfiguration_samples, uniform_included)
from __future__ import print_function
import time

from problog.logic import Constant,Term
from problog.program import PrologString
from files.evidence import getEvidences
from problog.tasks import sample

def main(width, height, turns, board_samples, uniform_included, samples):
    with open("files/model_problog.pl") as prologfile:
        logic_program = prologfile.read()
    logic_program += "\nwidth(" + str(width - 1) + ").\n"
    logic_program += "height(" + str(height - 1) + ").\n"
    logic_program += "query(score_of_turn(" + str(turns) + ",_)).\n"
    if (uniform_included):
        strategies = ['uniform', 'color_ratio', 'possible_score', 'possible_score_improved']
    else:
        strategies = ['color_ratio', 'possible_score', 'possible_score_improved']

    evidences = getEvidences("files/" + str(width) + "x" + str(height) + ".txt", board_samples)

    for strategy in strategies:
        perm_string = 'turn:' + str(turns) + ' strategy:' + strategy + ' size:' + str(width) + 'x' + str(height) + ' '
        logic_program_strategy = logic_program + "strategy(" + strategy + ").\n"
        evidence_index = 0

        average_score_dict_of_evidences = {}
        for evidence in evidences:
            average_score_dict_of_samples = {}
            logic_program_evidence = logic_program_strategy
            print('evidence: ' + str(evidence))
            for evidenceTerm in evidence:
                logic_program_evidence += "evidence" + str(evidenceTerm).lower() + ".\n"
            model = PrologString(logic_program_evidence)
            start_time = time.time()
            result = sample.sample(model, n=samples, propagate_evidence=True, progress=True)

            evidence_index += 1
            for i, r in enumerate(result):
                key = str(r)[16:17]
                if key in average_score_dict_of_samples:
                    average_score_dict_of_samples[key] += 1
                else:
                    average_score_dict_of_samples[key] = 1
                print('%s %i: %s' % ("sample",i,r))

            for keys, values in average_score_dict_of_samples.items():
                values /= float(samples)
                print('%s: %.12f' % ('average ' + perm_string + 'score of ' + keys, values))
            elapsed_time = time.time() - start_time
            print('%s %i: %.4fs' % (perm_string + 'samples evidence', evidence_index, elapsed_time))


if __name__ == '__main__':
    width = 3
    height = 3
    turns = 2
    boardconfiguration_samples = 2
    uniform_included = False
    samples = 5
    main(width, height, turns, boardconfiguration_samples, uniform_included, samples)
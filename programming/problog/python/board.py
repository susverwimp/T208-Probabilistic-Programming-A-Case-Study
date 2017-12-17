def printBoard(width, height, evidence):
    print('evidence:')
    board = [None] * height
    for i in range(0,height):
        row = []
        for j in range(0,width):
            row.append(str(evidence[i*width + j][0]).split(",")[0][6:])
        board[height - i - 1] = row
    s = [[str(e) for e in row] for row in board]
    lens = [max(map(len, col)) for col in zip(*s)]
    fmt = '\t'.join('{{:{}}}'.format(x) for x in lens)
    table = [fmt.format(*row) for row in s]
    print '\n'.join(table)

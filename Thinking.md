* move left/right:
    * out of matrix -> (OutOfMatrix, falling)
    * collision -> (Collision, falling)
    * ok -> (Ok, next)

* rotate left/right:
    * out of matrix -> (OutOfMatrix, falling) -> move left/right + rotate left/right
    * collision -> (Collision, falling) -> move left/right + rotate left/right
    * ok -> (Ok, next)

* move down:
    * 

spawning:
    * I, size 4, (10 - 4) / 2 = 3 -> offset = (3, 0)
    * L, size 3, (10 - 3) / 2 = 3 -> offset = (3, 0)
    * O, size 2, (10 - 2) / 2 = 4 -> offset = (4, 0)


rotateTwice:
    (0, 0) (0, 1) (0, 2)    (2, 2) (2, 1) (2, 0)    (size - row, size - col)
    (1, 0) (1, 1) (1, 2)    (1, 2) (1, 1) (1, 0)
    (2, 0) (2, 1) (2, 2)    (0, 2) (0, 1) (0, 0)
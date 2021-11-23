(Left  a <$$> Left  dd) e f = True
(Left  a <$$> Right d ) e f = True
(Right a <$$> Left  d ) e f = False
(Right a <$$> Right dd) e f = True

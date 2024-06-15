module TestFunctions ( 
    booth,
    bohachevsky,
    rosenbrock,
    rastrigin,
    holdertable,
    dropwave,
    eggholder,
    crossInTray,
    testFunctions
) where

-- https://www.sfu.ca/~ssurjano/booth.html
-- global min f(1,3) = 0
-- evaluated at x,y \in [-10,10]
booth :: (Num a, Floating a) => (a, a) -> a
booth v = (x+2*y-7)**2 + (2*x+y-5)**2
    where x = fst v
          y = snd v

-- https://www.sfu.ca/~ssurjano/boha.html
-- global min f(0,0) = 0
-- evaluated at x,y \in [-100,100]
bohachevsky :: (Num a, Floating a) => (a, a) -> a
bohachevsky v = x**2 + 2*(y**2) - a - b + 0.7
    where a = (0.3*cos (3*pi*x))
          b = 0.4*(cos (4*pi*y))
          x = fst v
          y = snd v

-- https://www.sfu.ca/~ssurjano/rosen.html
-- global min f(1,1) = 0
-- evaluated at x,y \in [-5,10]
rosenbrock :: (Num a, Floating a) => (a, a) -> a
rosenbrock v = (1-x)**2 + 100*(y - x**2)**2
    where x = fst v
          y = snd v

-- https://www.sfu.ca/~ssurjano/rastr.html
-- global min f(0,0) = 0
-- evaluated at x,y \in [-5.12,5.12]
rastrigin :: (Num a, Floating a) => (a, a) -> a
rastrigin v = a*n + b + c
    where a = 20
          n = 2
          b = x**2 - a * (cos $ 2*pi*x)
          c = y**2 - a * (cos $ 2*pi*y)
          x = fst v
          y = snd v

-- https://www.sfu.ca/~ssurjano/holder.html
-- global min f(a,b) = f(-a,b) = f(a,-b) = f(-a,-b) = -19.2085
-- where a = 8.05502, b = 9.66459
-- evaluated at x,y \in [-10,10]
holdertable :: (Num a, Floating a) => (a, a) -> a
holdertable v = - (abs a*b)
    where a = (sin x)*(cos y)
          b = exp $ abs $ 1 - (sqrt (x**2+y**2)/pi)
          x = fst v
          y = snd v

-- https://www.sfu.ca/~ssurjano/drop.html
-- global min f(0,0) = -1
-- evaluated at x,y \in [-5.12,5.12]
dropwave :: (Num a, Floating a) => (a, a) -> a
dropwave v = -(a/b)
    where a = 1 + (cos 12*(sqrt $ x**2+y**2))
          b = 0.5 * (x**2 + y**2) + 2
          x = fst v
          y = snd v

-- https://www.sfu.ca/~ssurjano/egg.html
-- global min f(512,404.2319) = -959.6407
-- evaluated at x,y \in [-512,512]
eggholder :: (Num a, Floating a) => (a, a) -> a
eggholder v = -a*b - x*c
    where a = y+47
          b = sin $ sqrt $ abs $ y + (x/2) + 47
          c = sin $ sqrt $ abs $ x - (y + 47)
          x = fst v
          y = snd v

-- https://www.sfu.ca/~ssurjano/crossit.html
-- global min f(a,b) = f(-a,b) = f(a,-b) = f(-a,-b) = -2.06261
-- where a = -1.3491, b = -1.3491
-- evaluated at x,y \in [-10, 10]
crossInTray :: (Num a, Floating a) => (a, a) -> a
crossInTray v = -0.001 * ((abs ((sin x) * (sin y) * a + 1)) ** (0.1))
    where a = exp $ abs $ 100 - (sqrt (x**2 + y**2))/pi
          x = fst v
          y = snd v

testFunctions = [
        ("booth", booth, ((-10, 10), (-10, 10)), (1, 3)),
        ("bohachevsky", bohachevsky, ((-100, 100), (-100, 100)), (0, 0)),
        ("rosenbrock", rosenbrock, ((-5, 10), (-5, 10)), (1, 1)),
        ("rastrigin", rastrigin, ((-5.12, 5.12), (-5.12, 5.12)), (0, 0)),
        ("holdertable", holdertable, ((-10, 10), (-10, 10)), (8.05502, 9.66459)),
        ("dropwave", dropwave, ((-5.12,5.12), (-5.12,5.12)), (0, 0)),
        ("eggholder", eggholder, ((-512, 512), (-512, 512)), (512, 404.2319)),
        ("crossInTray", crossInTray, ((-10, 10), (-10, 10)), (1.3491, 1.3491))
    ]


type SimpleState s a = s -> (a, s)

type StringState a = SimpleState String a

returnSt :: a -> SimpleState s a
returnSt a = \s -> (a, s)

returnAlt :: a -> SimpleState s a
returnAlt a s = (a, s)

bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindSt m k = \s -> let (a, s') = m s
                   in (k a) s'

-- m == step
-- k == makeStep
-- s == oldState
bindAlt step makeStep oldState =
    let (result, newState) = step oldState
    in (makeStep result) newState


bindAlt :: (s -> (a, s))      -- step
        -> (a -> s -> (b, s)) -- makeStep
        -> (s -> (b, s))      -- (makeStep result) newState

getSt :: SimpleState s s
getSt = \s -> (s, s)

putSt :: s -> SimpleState s ()
putSt s = \_ -> ((), s)

module Player where

import Physics

player :: Body -> Entity
player body = Entity {
	update time game = player (update time body)
	render = render body
}

playerUpdate :: Float -> Game -> Game
playerUpdate seconds game = game {player=pos'} where
    pos' = player game - 5*(keyboardToVector $ keyboard game)

stateToScalar :: KeyState -> Vector
stateToScalar Up = 1
stateToScalar Down = 0

keyToVector :: Key -> Vector
keyToVector (Char c) = maybe zero (j^) (elemIndex c "dwas")

keyboardToVector :: Map.Map Key KeyState -> Vector
keyboardToVector = Map.foldrWithKey f (0:+0) where
    f k v a = a + (stateToScalar v)*(keyToVector k)
module Entity where
import Graphics.Gloss (Picture)
import qualified Data.Map.Strict as Map
import Graphics.Gloss.Interface.Pure.Game (
      Key(Char)
    , KeyState(Up, Down)
    )

import Player

data Entity = Entity {
	update :: Float -> Game -> Entity
	render :: Game -> Picture
}

data Game = Game
    { player :: Player,
    , keyboard :: Map.Map Key KeyState
    }
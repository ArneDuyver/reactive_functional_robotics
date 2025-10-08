{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Helpers.Turtlebot where

import Data.Bool (Bool (False))
import GHC.Generics (Generic)

-- EXTRA FUNCTIONS 
-- Pos vel is forward, pos rot is anti-clockwise turn
translationalAndRotationalVelocitiesToWheelVelocities :: Double -> Double -> Double -> Double -> (Double, Double)
translationalAndRotationalVelocitiesToWheelVelocities transVel rotVel wheelDistanceInM wheelRadiusInM =
  let -- Clamp input velocities to allowed ranges
      rotVelInverse = -rotVel
      clampedTrans = clamp (-maxTrans) maxTrans transVel
        where maxTrans = 1
      
      clampedRot = clamp (-maxRot) maxRot rotVelInverse
        where maxRot = 3.14 * 2
      
      -- Correct calculation using full wheel distance
      wheelDist = wheelDistanceInM
      leftVelRaw = (clampedTrans + (clampedRot * wheelDist / 2)) / wheelRadiusInM 
      rightVelRaw = (clampedTrans - (clampedRot * wheelDist / 2)) / wheelRadiusInM 
      
      -- Clamp wheel velocities to motor limits
      maxMotor = 3.14 * 20
      leftVel = clamp (-maxMotor) maxMotor leftVelRaw
      rightVel = clamp (-maxMotor) maxMotor rightVelRaw
  in (leftVel, rightVel)
  where
    clamp :: Double -> Double -> Double -> Double
    clamp lower upper = max lower . min upper

getTranslationalAndRotationalVelocities :: Double -> Double -> Double -> Double -> (Double, Double)
getTranslationalAndRotationalVelocities motorVelLeftRadSec motorVelRightRadSec wheelDistanceInM wheelRadiusInM = (vel, rot)
  where v_l = motorVelLeftRadSec * wheelRadiusInM
        v_r = motorVelRightRadSec * wheelRadiusInM
        vel = (v_r + v_l)/2
        rot = ((-1)*(v_r - v_l))/wheelDistanceInM

degreesToRadians :: Double -> Double
degreesToRadians deg = (deg / 180) * pi

radiansToDegrees :: Double -> Double
radiansToDegrees rad = (rad / pi) * 180


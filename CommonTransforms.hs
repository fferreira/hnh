{-
  This file is part of HNH.

    HNH is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    HNH is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
    
    Copyright 2010 Francisco Ferreira
-}
module CommonTransforms
  (
    commonTransforms
  )
  where
  
import Syntax
import TransformMonad(TransformM)

import ExprTransformer(correctPrecedence, toPrefix,literalStringElimination)
import TreeSimplify(funToLambda, simplifyLambda, simplifyPatterns)

commonTransforms :: Program -> TransformM Program
commonTransforms p = correctPrecedence p 
                     >>= literalStringElimination
                     >>= toPrefix
                     >>= funToLambda
                     >>= simplifyPatterns
                     >>= simplifyLambda


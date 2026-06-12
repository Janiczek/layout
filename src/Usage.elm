module Usage exposing (main)

import Html exposing (Html)
import Examples
import Layout
import Render

main : Html msg
main =
  Examples.ex1
  |> Layout.layout
  |> Render.webgl


intent : String
intent =
    """
Immediate mode GUI in Elm via elements positioned
strictly by transform: translate(x,y).

https://youtu.be/by9lQvpvMIc?t=837

Left/top offset = root padding left/top
For each child:
    - child position = root position + child position
    - child position x/y += left/top offset
    - draw
    - left/top offset += child width/height + root child gap


On visiting an element:

On closing an element:
  - element width += left padding + right padding
  - element height += top padding + bottom padding
  - ??? https://youtu.be/by9lQvpvMIc?t=1030
    - child gap = (parent child count - 1) * parent child gap
  - depending on parent direction LR/TB:
    - element width/height += child gap
    - parent width/height += element width/height
    - parent height/width = max(parent height/width, element height/width)
    - parent minWidth/minHeight += element minWidth/minHeight
    - parent minHeight/minWidth += max(child minHeight/minWidth, parent minHeight/minWidth)
Said more declaratively,
- parent size along layout axis = Sum of children
- parent size across layout axis = Max of children

Going down (with children sizes being already done):
  parent width = parent left padding + parent right padding + sum of child widths + (children.length - 1) * parent childGap
  parent height = parent top padding + parent bottom padding + max of child heights
  (or switched if direction TB)

1. Fit sizing widths (depth first post order?)
2. Grow & shrink sizing widths (breadth first)
3. Wrap text
4. Fit sizing heights (depth first post order?)
5. Grow & shrink sizing heights (breadth first)
6. Positions & alignment
7. Draw

Width of grow() = technically 0 but could represent it as "Hasn't grown yet"

Grow child elements:
  - remaining width = parent width - left+right paddings - child widths - all gaps
  - give the remaining width to all GROW children (not set! redistribute to them, +=)

  - remaining height = parent height - top+bottom paddings
  - give the remaining height to the GROW children

Grow = while there is some remaining width: (https://youtu.be/by9lQvpvMIc?t=1590)
  - expand the smallest elements until they reach the size of the next smallest elements
Shrink = dual
  - when we find a shrinkable element (one of the currently largest) is already
    at its min size, remove it from the list

Text preferential width = if it had unlimited space available
Text minimum width = width of its largest word?

Shrinking: dual of growing
shrink largest elements (that aren't their minimum widths)
at the same rate until you hit the next largest thing

Need a way to measure text bbox
    """

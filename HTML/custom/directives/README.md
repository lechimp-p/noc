### autoresize

Attach it as attribute to a textarea. Will take care that the textarea automatically
resizes on growing user input.

### sticky

Attach it as an attribute to a block element. Will measure the vertical offset to
element with id defined as attribute stick-to. If offset is 0, the element is
replaced by a placeholder element (with the class defined as attribute placeholder-
class) with the same height as the original element. The class defined as sticky-class
gets attached to the element. If the placeholders offset is larger than 0 again,
the placeholder is purged and the sticky-class is removed from the original element.

sticky              -- Activate sticky behaviour.
sticky-class        -- Class to apply to the element when sticking.
                       Defaults to 'sticky'
placeholder-class   -- Class to be used for the placeholder of the sticking element.
                       Defaults to 'unsticky'
stick-to            -- Id of the element to measure relative offset. Defaults to window.
offset              -- An optional integer value to modify the amount of offset needed
                       to make element stick. Defaults to 0.

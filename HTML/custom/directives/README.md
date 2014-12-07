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

**Attributes:**

sticky              -- Activate sticky behaviour.
sticky-class        -- Class to apply to the element when sticking.
                       Defaults to 'sticky'
placeholder-class   -- Class to be used for the placeholder of the sticking element.
                       Defaults to 'unsticky'
stick-to            -- Id of the element to measure relative offset. Defaults to window.
offset              -- An optional integer value to modify the amount of offset needed
                       to make element stick. Defaults to 0.

### throttled-update

Attach to an element as an attribute. Will watch for changes on the model property
defined via ng-model. If the property changes, will set the updating class on the
element and set the updating property on the scope to true. After a delay of 1s after
the last change on the model.property, it calls update(model-property) on the scope.

Expects scope to have a property update taking a string name for the property to 
update and returning a promise. update could e.g. perform the update of the property 
on the server.

**Attributes:**

ng-model                - The property to watch.
update-class            - The class to be added to the element when updating. Defaults 
                          to "updating".
error-class             - The class to set on the element when update was faulty. 
                          Defaults tp "faulty".
update-handler          - The property to call on the scope after delay. Will be
                          called with the property defined via ng-model.
**Scope variables:**

updating                - True when update is in progress, false otherwise.
error                   - True when last update attempt was faulty. False otherwise or
                          when updating is true.


### confirmed-update

Attach to an element as an attribute. Will watch for changes on the model property
defined via ng-model. If the property changes, will set the confirmation class on the
element and set the confirmation property on the scope to true. When confirm is called
the update is actually performed in a similar way to throttled-update. When abort is
called, the property is reset.

Expects scope to have a property update taking a string name for the property to 
update and returning a promise. update could e.g. perform the update of the property 
on the server.

**Attributes:**

ng-model                - The property to watch.
confirmation-class      - The class to be added to the element when confirmation is needed. 
                          Defaults to "confirmation".
update-class            - The class to be added to the element when updating. Defaults 
                          to "updating".
error-class             - The class to set on the element when update was faulty. 
                          Defaults tp "faulty".
update-handler          - The property to call on the scope after delay. Will be
                          called with the property defined via ng-model.
**Scope variables:**

confirmation            - True when confirmation is needed, false otherwise.
updating                - True when update is in progress, false otherwise.
error                   - True when last update attempt was faulty. False otherwise or
                          when updating is true.


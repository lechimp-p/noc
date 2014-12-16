### channel-controller

channel
    id
    name
    description
    type                -- none, stream oder conversation
    image
    amountOfUsers
    lastPost            -- Zeitpunkt des letzten Posts
    subscribed          -- Ist der aktuelle Benutzer subscribed?
    owner               -- Ist der aktuelle Benutzer ein Owner des Kanals?
    producer            -- Ist der aktuelle Benutzer ein Producer des Kanals?
    consumer            -- Ist der aktuelle Benutzer ein Consumer des Kanals?

msgs
    msg
        image
        text
        timestamp
        author          -- Analog zu user aus profile-controller

message                 -- Für Input.

post()                  -- Postet Inhalt von message in den Channel.
subscribe()             -- Subscribed den aktuellen Benutzer im Channel.
unsubscribe()           -- Unsubscribed den aktuellen Benutzer vom Channel.


### channel-overview-controller

channels
    channel             -- Analog zu channel aus channel-controller


### contact-overview-controller

contacts
    contact             -- Analog zu user aus profile-handler
        lastMessage     -- Analog zu message aus channel-controller.


### error-controller


### login-controller

username                -- Für Input.
password                -- Für Input.

login()                 -- Führt Login mit username und password aus.


### my-profile-controller

user                    -- Analog zu user aus profile-controller
    email

### profile-controller

user
    id
    login
    name
    description
    icon
    contact             -- Id des Channels zwischen dem aktuellen Benutzer und dem 
                           Kontakt, wenn Kontakt besteht.
    adding              -- true, wenn Benutzer gerade zu Kontakten hinzugefügt wird
                           sonst false.
    removing            -- true, wenn Benutzer gerade aus Kontakten entfernt wird, 
                           sonst false.
    addRemoveError      -- true, wenn es einen Fehler beim Hinzufügen oder Entfernen
                           gab.

addToContacts()         -- Fügt den Benutzer zu den Kontakten des aktuellen Benutzers
                           hinzu.    
removeFromContacts()    -- Entfernt den Benutzer aus den Kontakten des aktuellen
                           Benutzers.

### notifications-controller

notifications
    notification
        type            -- Im Moment nur 'added-to-channel'.
        timestamp
        user            -- Analog zu user aus profile-controller.
        channel         -- Analog zu channel aus channel-controller.
      

### topbar-controller

user                    -- Analog zu user aus profile-controller für den eingeloggten
                           Benutzer.

logout()                -- Loggt den Benutzer aus.

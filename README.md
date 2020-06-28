# YeeHAA
**Yee**light **H**ighlevel **A**PI for local network **A**pplications

## Background
Yeelight can be controlled by both its own application and Mi Home. However, these apps do the control through cloud servers.
That means when you ask, say, to toggle on/off, your request goes somewhere out there first before reaching your bulb, possibly
to a server in a country far, far away depending on which server you choose. Yeelight provides LAN control, so that it can be
controlled through local network (i.e. your application and the bulbs are connected to the same network). However, there's no
official applications built for this, hence this project.

This project tries to provide an API, both as a Pascal unit and a dynamic library with C style interface so that it can be
called by any other languages capable of interfacing with C. The Pascal unit is the preferable way if you want to write the
application in Pascal, of course.

## Status
POC: Working prototype capable of discovering bulbs and print some of their properties

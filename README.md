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

### Working

* Bulb discoveries
* Buld properties (partial, only some that are interesting to me)
* Set name
* Set power state
* Set color temperature
* Set color RGB
* Set brightness

### To do
* The rest of available commands

API is considered unstable as of now, it may break anytime as I find "better" interface as development goes

## Requirements and Dependencies

* FPC 3.X.X
* Lazarus to build the GUI demo and ease building the library and unit
* lNet (lnetbase to be precise, install from Lazarus' online package manager)
* laz_synapse (install from Lazarus' online package manager)

## How to Build

1. Open yeehaa.lpg if you have project group package installed in Lazarus, alternatively go to any of the subdirectory library, clidemo or guidemo
2. If you open the project group, choose one of the projects in project group window, otherwise, open the .lpi in the subdirectory
3. Compile Ctrl+F9, optionally choosing build mode (Debug/Release)

## Directories

### library

Contains a project to build the dynamic library and a C interface alongside a C example

### clidemo

Contain a demo CLI project that uses the unit directly

### guidemo

Contain a demo GUI project that uses the unit directly

### androiddemo

Contain a demo Android project that uses the unit directly

---

## Technical Details

### Official Documentation

https://www.yeelight.com/download/Yeelight_Inter-Operation_Spec.pdf

### Broadcast Request Message

```
M-SEARCH * HTTP/1.1
HOST: 239.255.255.250:1982
MAN: "ssdp:discover"
ST: wifi_bulb
```

### Sample Broadcast Response

```
HTTP/1.1 200 OK
Cache-Control: max-age=3600
Date:
Ext:
Location: yeelight://192.168.1.104:55443
Server: POSIX UPnP/1.0 YGLC/1
id: 0x0000000007f1c9c8
model: color
fw_ver: 65
support: get_prop set_default set_power toggle set_bright start_cf stop_cf set_scene cron_add cron_get cron_del set_ct_abx set_rgb set_hsv set_adjust adjust_bright adjust_ct adjust_color set_music set_name
power: off
bright: 100
color_mode: 2
ct: 6500
rgb: 16711680
hue: 359
sat: 100
name:
```

## Rant

To be honest, I'm not quite sure how the discovery protocol actually works and I still consider my implementation as suboptimal. It does work, but it does polling which I don't really like.
The documentation seems to indicate some kind of push notification, but I can't make it work just yet. Patches are welcome if you can implement it as expected.

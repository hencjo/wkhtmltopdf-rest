#!/bin/bash
curl -v http://localhost:8000 -d src="https://www.google.se" -d username='henrik@hencjo.com' -d key='RzNIKegEXLOt44WwRsx3OH5ZPZiMkKLo' -d page-size=A4  > meow.pdf && zathura meow.pdf


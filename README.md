This is a simple version of Conway's Game Of Life written in Common Lisp, using OpenGL and SDL.

Here is a sample usage and screenshot:

```commonlisp
* (ql:quickload 'gllife)
To load "gllife":
  Load 1 ASDF system:
    gllife
; Loading "gllife"

(GLLIFE)
* (gllife:start-life :board-width 200 :board-height 200)
```

![Screenshot](http://www.laroccophoto.com/photos/i-2jCWGSw/0/X3/i-2jCWGSw-X3.png "GLLife Screenshot")


Today I started to look again into a simple way to deploy anaconda with Python 3 and Qt. After updating conda, anaconda (my 2.7 and 3.3 environments) to 1.9
I came across a promising package in binstar: pyside-pyzo. Knowing that pyzo is Python3, I gave it a try:

$ conda install -c https://conda.binstar.org/pyzo pyside-pyzo --prefix=/Users/XXX/anaconda/envs/py33

The installation worked:

$ conda list -n py33 | grep pyside
pyside-pyzo               1.2.1                    py33_4

And to my surprise, everything that I have tried worked fine. Would it be possible to incorporate this into anaconda?

==================================================================================
We do plan to integrate the pyzo package eventually. There are some
issues. Currently, pyzo-pyside includes its own qt, but it would be
better to have it separated like it is in Anaconda.

By the way, on Linux and OS X you can use my build of pyqt, which is
available for all versions of Python. 

$ conda install -c asmeurer pyqt.

Aaron Meurer
- show quoted text -
> --
> Anaconda Community Support Group Brought to you by Continuum Analytics
> ---
> You received this message because you are subscribed to the Google Groups
> "Anaconda - Public" group.
- show quoted text -

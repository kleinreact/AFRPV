# Arrowized Functional Reactive Program Visualization

The project presents a first proof of concept implementation of the
idea to visualize the network of arrowized functional reactive
programs. The advantage of arrowized reactive programs against other
FRP systems is that they provide a well defined graphical
representation by construction. The underlying representation
immediately follows from the creation of the program via the core
arrow operations as well as library specific primitives, such as
switches and others.

To extract the program structure, our implementation does not run a
static analysis of the source code, since this would introduce a huge
implementation burden due to parsing requirements and so forth.
Instead, we use the extendability of the arrow classes to create a
wrapper arrow that extends the original arrow definition of the
library with an observer that records the created network during
creation. With this observer at hand, we then also created a wrapper
module specific for the AFRP library, that provides the exact same
interface. This way, the visualizer can be easily embedded, even into
existing projects, by just importing the wrapper module instead of the
original one. After creation, the network then can be plotted using
one of the supported backends.

## Supported Libraries

* Yampa-0.11

## Supported Rendering Backends

* Tikz + pdflatex

## Limitations

* Due to their dynamic nature, we cannot visualize switches within the
  network to their full extend yet. Since the network that is switched
  to is dynamically created at runtime it's graphical representation
  is also only available at that point. Thus, at the moment we only
  support visualization of the network that is switched from, as this
  can be statically determined during the initial arrow creation.

* Yampa's `pause`, `rSwitch`, `kSwitch`, and parallel switches are not
  supported yet.

* Using specific methods from `FRP.Yampa.(...)` sub-modules is
  currently not supported (may work for some of the sub-modules, but
  not tested specifically yet).

## Requirements

* `stack` or `cabal + ghc + package dependencies`
* `pdflatex` with `tikz` package
* a Yampa program for visualization that supports Yampa-0.11

## Usage

* Replace all imports of `FRP.Yampa` in your program with `AFRPV.Yampa`.
* Use the new function `renderNetwork` to plot the signal function
  (see the haddock documentation for details).
* You also may run the `legend` executable to get a short explanation
  of the different visualized primitives. Executing the tool creates an
  overview PDF under `legend/legend.pdf"

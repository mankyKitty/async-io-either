Async IO Either
===============

This is a tiny package that captures an pattern I've been using a lot...

Essentially I am tired of needing something of (IO a) and it so often coming with exceptions
that are not indicated in the type. Generally speaking my application, like many others, is
just going to throw in the towel when an exception occurs, but I would prefer it if a value was
provided. This would then allow me to decide just what sort of tantrum my application will throw.

I've tried to understand the different manners of handling exceptions in Haskell, and I'm sure there
is a better or more fitting approach. But this is what I have for now.

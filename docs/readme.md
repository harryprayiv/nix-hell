# # Nix-Hell: A Typed Shell for the Nix Ecosystem 😈

Nix-Hell is a fork of the hell shell scripting language (a tiny dialect of Haskell) expanded to be a domain specific nix scripting language.


See [homepage](https://chrisdone.github.io/hell/) for documentation, downloads, architecture, etc.


# # Back Story:

I have been writing shell scripts for as long as I have been writing software, and I have never liked it. Bash is remarkable in the sense that it has survived decades of abuse and still mostly works, but let us be honest: it is a terrible programming language. The quoting rules are a disaster. The error handling is opt-in and routinely forgotten. The type system is "everything is a string, good luck."

This bothers me more than usual when I am working in Nix. You have this extraordinary system for reproducible, typed, compositional software construction, and then right at the edges of it, where the actual work gets done, you have bash scripts. The Nix language is purely functional and quite elegant. The builds are hermetic, the dependency graph is tracked with mathematical precision. And then you write your deployment scripts and you are back in 1989.

A few days ago I found Chris Done's Hell: take the Haskell type checker, strip out everything that makes Haskell complicated, and expose the result as a scripting language. The entire implementation fits in one file. You write do-notation, call processes, manipulate files, and the type checker tells you when you have made a mistake before you run the script on your production database.

So I am forking Hell and calling it Nix-Hell. The core language stays exactly as Chris designed it. What I am adding is a layer of Nix-specific primitives and types: a `StorePath` type that the checker distinguishes from plain text, a `Secret` type that cannot accidentally flow into a log message, and first-class primitives for store operations, flake evaluation, and system management.

The great insight I am borrowing from Hell is that scripting languages do not need to be powerful. They need to be correct. A language that distinguishes `StorePath` from `Text` and `Secret` from `Text` is already dramatically safer than bash without needing anything more sophisticated than a simply-typed lambda calculus.

The Nix ecosystem deserves a scripting language that understands it. Hell gave us the foundation. Nix-Hell is what you build on top.

Code coming shortly. Collaborators welcome.

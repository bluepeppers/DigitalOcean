DigitalOcean
============

A Haskell Digital Ocean API.

Exposes a lens based interface onto the Digital Ocean API. Fairly feature
complete.

Error Handling & IO Monads
--------------------------

Instead of enforcing a monad to return, all functions that do IO and stuff have a
type something along the lines of:

    getSSHKeys :: (Error e, MonadError e m, MonadIO m) => Config -> m [SSHKey]

Translated to human, this just means that the monad `m` that is returned can
1) encapsulate IO, and 2) represent an error, of type `e`. This is useful if you
want to use `ExceptT` or something similarly fun to represent your errors.
However, for simple use, you will be relived to hear that `m` may be the plain
old `IO` monad, which will represent its errors by throwing `IOException`
exceptions.

This means, despite the confusing type signature, we may write things such as:

    main = do
      auth <- fmap head getArgs -- Get the auth from the cmd line
      keys <- getSSHKeys . Config . pack $ auth -- Get the ssh keys
      putStrLn . show $ keys -- And display them

See the ./examples directory for further usage

TODO:
-----

* Pagination
* Tests
* HTTP session reuse

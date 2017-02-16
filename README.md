# fitR

An experimental package to read .fit files produced by fitness tracking devices 
like Garmin Edge cycle computers or sports watches.  Other R packagesfor this task already exist,
but these seem to rely on external code released as part of the FIT SDK.  The intention
for **fitFileR** is to use native R code to read the files directly, and do away with
the dependence on code written in other languages. As such **fitFileR** is very much a work
in progress, and many features available in the complete SDK are not currently implemented.

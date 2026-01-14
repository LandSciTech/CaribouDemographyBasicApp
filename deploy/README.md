# Instructions for bundling a new version of the app

Bundling the app allows users to use the app without installing R or having to run any R code.
It also encapsulates the package versions that they will have, which should help
avoid issues with changes to dependency packages.

NOTE! The bundled version of the app is set up to run without rtools, which is a
requirement of nimble, so in this version users can not update the data from
google sheets or re-run the bboutools models. It is possible to create a bundle
with rtools but it is very large.

0) It is a good idea to increment the version number of the package before
deploying so that the version is clear. Run `usethis::use_version()`

1) in the depoly folder double click build.bat. This will take ~10 minutes to
run and should result in a caribou_demography_app.tar folder being created in
the deploy folder.

2) in the GitHub repo go to [Releases](https://github.com/LandSciTech/CaribouDemographyBasicApp/releases)
and click "Draft a new release".

3) Choose a tag that matches the version number of the package

4) Copy the text from README-USER.txt to the Release description.

5) Upload the caribou_demography_app.tar file in the area that says "Attach binaries ...".

6) Check the "Set as a pre-release" box to indicate that the app is still in development

7) Click publish release.

8) Send the link to the releases page to any users so that they will always see
the most recent release.


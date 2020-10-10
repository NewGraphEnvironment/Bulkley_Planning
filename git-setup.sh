##we use this file to set up our lfs for the photos and other large files that we do not want to fill up our repo
##becasue github has limits on the size of repos

git lfs track "*.jpg"
git lfs track "*.JPG"
git lfs track "*.png"
git lfs track "*.PNG"


##add an attributes file
git add .gitattributes

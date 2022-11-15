set ProjectName=Buki
set exename=Buki

echo %ProjectName% > readme.md

git init
git add readme.md
git commit -m "first commit"
git branch -M main
gh repo create %exename% --public --source=. --description "%ProjectName%"

git push -u origin main

gh release create 1.0 --notes "initial release"

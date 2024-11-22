# macroalgae-index
Calculating macroalgae indices and EQR values


https://macroalgae.p.niva.no/


# macroalgae app deployment

Private deployment for the public repository [martini](https://github.com/NIVANorge/macroalgae-index). 

You have to manually select the tag of the image, which is automatically deployed when the tag change is committed on github.  

1. Select your image tag from [artifact registry](https://console.cloud.google.com/artifacts/docker/niva-cd/europe-west1/images/macroalgae?project=niva-cd)
2. Update the newTag field for macroalgae-index in  [kustomization.yaml](https://github.com/NIVANorge/nivacloud-manifests/blob/main/workloads/macroalgae/base/kustomization.yaml) in the [nivacloud-mainifests repository](https://github.com/NIVANorge/nivacloud-manifests)

3. Commit and push
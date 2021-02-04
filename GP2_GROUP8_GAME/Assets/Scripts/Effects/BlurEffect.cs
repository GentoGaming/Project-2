using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.Universal;

public class BlurEffect : MonoBehaviour
{
    private Volume volume;
    DepthOfField depthOfField;
    float parameterValue = 0f;
    public float timeBeforeBlurRemoval = 4.5f;
    public float increaseProduct = 0.1f;
    public float targetFocusDistance = 10f;

    // Start is called before the first frame update
    void Start()
    {
        volume = GetComponent<Volume>();

        DepthOfField depth;

        if (volume.profile.TryGet<DepthOfField>(out depth))
        {
            depthOfField = depth;
        }

        StartCoroutine(blurSequencer());
    }
    IEnumerator blurSequencer()
    {
        float currentInitiationTime = 0f;
        while(currentInitiationTime < timeBeforeBlurRemoval)
        {
            currentInitiationTime += Time.deltaTime; 
            yield return null;
        }

        while(depthOfField.focusDistance.value < targetFocusDistance) 
        {
            depthOfField.focusDistance.value += Time.deltaTime * increaseProduct;
            yield return null;
        }

        volume.profile.Remove<DepthOfField>();
       
    }
}

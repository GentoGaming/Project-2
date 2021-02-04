//@Author: Teodor Tysklind / FutureGames / Teodor.Tysklind@FutureGames.nu

using System.Collections;
using UnityEngine;

public class SafeBehaviour : MonoBehaviour
{
    [SerializeField] private GameObject particleObject;
    [SerializeField] private GameObject safeHatch;

    public void OpenSafe()
    {
        StartCoroutine(OpenHatch());
    }

    private IEnumerator OpenHatch()
    {
        while (safeHatch.transform.localRotation.eulerAngles.y < 180)
        {
            safeHatch.transform.Rotate(Vector3.up);
            yield return null;
        }
    }
}

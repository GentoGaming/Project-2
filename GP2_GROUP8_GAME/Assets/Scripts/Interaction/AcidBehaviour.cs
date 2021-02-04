//@Author: Teodor Tysklind / FutureGames / Teodor.Tysklind@FutureGames.nu

using System.Collections;
using UnityEngine;

public class AcidBehaviour : GrabbableBehaviour
{
    private Camera _main;
    private bool _inRange = false;
    private CheckForInteractable _checkForInteractable;
    
    [FMODUnity.EventRef]
    public string chemistrySizzle = "event:/Environmental/chemistrySizzle";
    [FMODUnity.EventRef]
    public string Success = "event:/Music/Success";


    private SafeBehaviour _closestSafe;

    private void Start()
    {
        _checkForInteractable = GameManager.instance.player.GetComponent<CheckForInteractable>();
        _main = Camera.main;
    }

    protected override IEnumerator UpdateGrabPosition()
    {
        while (_isGrabbed)
        {
            SetPosition();
            CheckForSafeInteraction();

            yield return null;

            if (Input.GetMouseButtonDown(0))
            {
                Release();
            }
        }
    }

    //TODO: THIS METHOD IS HORRIBLY SCUFFED
    private void CheckForSafeInteraction()
    {
        if (_checkForInteractable.LastHighlightedGameObject == gameObject)
        {
            _checkForInteractable.RemoveHighlight();
        }

        Ray ray = new Ray(_main.transform.position, _main.transform.forward);
        RaycastHit[] hits = Physics.SphereCastAll(ray, 1f, _checkForInteractable.interactionRange);

        _inRange = false;

        foreach (RaycastHit hit in hits)
        {
            if (hit.transform.GetComponent<SafeBehaviour>() != null)
            {
                HighlightObject(hit.transform.gameObject);
                _closestSafe = hit.transform.GetComponent<SafeBehaviour>();
                _inRange = true;
            }
        }
    }

    private void HighlightObject(GameObject go)
    {
        if (_checkForInteractable.LastHighlightedGameObject != go)
        {
            _checkForInteractable.HighLightObject(go);
        }
    }

    protected override void Release()
    {
        if (_inRange)
        {
            _closestSafe.OpenSafe();
            FMODUnity.RuntimeManager.PlayOneShot(chemistrySizzle, transform.position);
            FMODUnity.RuntimeManager.PlayOneShot(Success);
            Destroy(gameObject);
        }
        else
        {
            base.Release();
        }
    }

}

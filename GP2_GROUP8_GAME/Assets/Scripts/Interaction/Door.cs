using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Door : MonoBehaviour, IInteractable
{
    [SerializeField] private float     rotation;
    [SerializeField] private float     dampening;
    private                  Transform _parentTransform;
    private                  bool      _open;
    private                  bool      _rotating;
    
    private void Start()
    {
        _parentTransform = transform.parent;
    }
    
    public void Interact()
    {
        if (!_rotating)
        {
            if (!_open)
            {
                StartCoroutine(RotateObject());
            }
            else
            {
                StartCoroutine(ResetRotation());
            }
        }
    }

    public IEnumerator ResetRotation()
    {
        _rotating = true;
        float startTime = 0.1f;
        while(startTime < 3)
        {
            transform.localRotation = Quaternion.Lerp(transform.localRotation, Quaternion.Euler(0, 0, 0), (startTime += Time.deltaTime)/dampening);
            yield return null;
        }
        _open = false;
        _rotating = false;
    }
    
    public IEnumerator RotateObject()
    {
        _rotating = true;
        float startTime = Time.time;
        while(Time.time < startTime + 2f)
        {
            transform.localRotation = Quaternion.Lerp(transform.localRotation, Quaternion.Euler(0, rotation, 0), (Time.time - startTime)/dampening);
            yield return null;
        }

        _open = true;
        _rotating = false;
    }
}

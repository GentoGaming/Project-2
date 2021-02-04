//@Author: Teodor Tysklind / FutureGames / Teodor.Tysklind@FutureGames.nu

using System.Collections;
using UnityEngine;
using UnityEngine.Assertions;

public class ChemicalBehaviour : GrabbableBehaviour
{
    [SerializeField] public ChemicalType _chemicalType;
    [SerializeField] private GameObject _substance;

    //TODO: DECOUPLE
    private CheckForInteractable _checkForInteractable;
    private Camera _main;
    private CameraController _cameraController;
    private bool _inRange = false;

    private BurnerBehaviour _closestBurner;
    private Animator _animator;
    private Transform _playerTransform;

    private Vector3 _pourPosition;

    private float t;
    private float startPitch;
    private float endPitch;

    private Quaternion startPlayerRotation;
    private Quaternion endPlayerRotation;

    private Vector3 startPosition;
    private Vector3 endPosition;

    private Quaternion startObjectRotation;
    private Quaternion endObjectRotation;

    private void Start()
    {
        if (_substance != null)
        {
            Assert.IsNotNull(_substance);
            _animator = gameObject.GetComponent<Animator>();
        }
     
        _checkForInteractable = GameManager.instance.player.GetComponent<CheckForInteractable>();
        _main = Camera.main;
        _playerTransform = GameManager.instance.player.transform;
        _cameraController = Camera.main.GetComponentInParent<CameraController>();
    }

    protected override IEnumerator UpdateGrabPosition()
    {
        while (_isGrabbed)
        {
            SetPosition();
            CheckForBurnerInteraction();

            yield return null;


            if (Input.GetMouseButtonDown(0))
            {
                if (_inRange)
                {
                    AddToCompound();
                }
                else
                {
                    Release();
                }
            }
        }
    }

    //TODO: THIS METHOD IS HORRIBLY SCUFFED
    private void CheckForBurnerInteraction()
    {
        if (_checkForInteractable.LastHighlightedGameObject == gameObject)
        {
            _checkForInteractable.RemoveHighlight();
        }

        Ray ray = new Ray(_main.transform.position, _main.transform.forward);
        RaycastHit[] hits = Physics.SphereCastAll(ray, 1f, _checkForInteractable.interactionRange, _checkForInteractable.layer);

        _inRange = false;

        foreach (RaycastHit hit in hits)
        {
            if (hit.transform.GetComponent<BurnerBehaviour>() != null)
            {
                HighlightObject(hit.transform.gameObject);
                _closestBurner = hit.transform.GetComponent<BurnerBehaviour>();
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

    private void AddToCompound()
    {
        Pour();
    }

    private void Pour()
    {
        PlayerInput.instance.NullifyInput = true;
 
        Vector3 direction = _closestBurner.transform.position - _playerTransform.position;
        Vector3 up = _playerTransform.transform.up;

        Vector3 cross = Vector3.Cross(direction.normalized, up);
        
        Vector3 pourPosition = _closestBurner.transform.position + (cross * 0.2f) + transform.up * 0.4f;
        _pourPosition = pourPosition;

        _animator = gameObject.GetComponent<Animator>();
        _rigidbody.isKinematic = true;
        transform.parent = null;
        
        Cursor.lockState = CursorLockMode.Locked;

        _isGrabbed = false;
        //StartCoroutine(TurnAround());
        StartCoroutine(AnimatePositioning());
    }

    private IEnumerator RunPourAnimation()
    {
        _animator.SetTrigger("Pour");
        Debug.Log("Hey");
        
        while (_animator.GetCurrentAnimatorStateInfo(0).IsName("Idle"))
        {
            yield return null;
        }
        
        while (_animator.GetCurrentAnimatorStateInfo(0).IsName("Pouring"))
        {
            yield return null;
        }
        
        _closestBurner.UpdateLiquid(_chemicalType);
        PlayerInput.instance.NullifyInput = false;
        Destroy(gameObject);
    }

    private IEnumerator TurnAround()
    {
        Vector3 targetDirection = _closestBurner.transform.position - _playerTransform.position;

        float speed = 0.5f;
        float singeStep = speed * Time.deltaTime;


        bool done = false;

        while (!done)
        {
            bool cameraInPosition = SnapCamera();
            bool rotationInPosition = SnapRotation();
            bool objectInPosition = SnapToPosition();
            
            if (cameraInPosition && rotationInPosition && objectInPosition)
            {
                done = true;
            }

            yield return null;

        }

        StartCoroutine(RunPourAnimation());
    }

    private bool SnapToPosition()
    {
        
        Vector3 startPosition = transform.position;
        float xVal = 0;
        
        xVal += Time.deltaTime * 1f;
        Mathf.Clamp01(xVal);
        
        if (Vector3.Distance(transform.position, _pourPosition) < 0.001f)
        {
            return true;
        }
        else
        {
            transform.position = Vector3.MoveTowards(transform.position, _pourPosition, Time.deltaTime * 3f);
            return false;
        }
    }

    private bool SnapRotation()
    {
        Vector3 burnerDirection = _closestBurner.transform.position - _playerTransform.position;
        Vector3 targetDirection = new Vector3(burnerDirection.normalized.x, 0f, burnerDirection.z);
        
        float speed = 0.8f;
        float singeStep = speed * Time.deltaTime;
        
        if (Vector3.Dot(targetDirection.normalized, _playerTransform.forward) > 0.99f)
        {
            return true;
        }
        else
        {
            Vector3 newDirection = Vector3.RotateTowards(_playerTransform.forward, targetDirection, singeStep, 0.0f);
            _playerTransform.rotation = Quaternion.LookRotation(newDirection);
            return false;
        }
    }

    private IEnumerator AnimatePositioning()
    {
        Vector3 burnerDirection = _closestBurner.transform.position - _playerTransform.position;
        Vector3 targetDirection = new Vector3(burnerDirection.normalized.x, 0f, burnerDirection.z);
        Quaternion targetRotation = Quaternion.LookRotation(targetDirection, Vector3.up);
        
        t = 0;
        
        startPitch = _cameraController.Pitch;
        endPitch = 0;
        startPlayerRotation = _playerTransform.rotation;
        endPlayerRotation = targetRotation;
        startPosition = transform.position;
        endPosition = _pourPosition;
        startObjectRotation = transform.rotation;
        endObjectRotation = _playerTransform.rotation;

        while (t < 1)
        {
            t += Time.deltaTime * 5f;
            
            InterpolateInPosition();
            InterpolateYRot();
            InterpolateObjectPosition();
            InterpolateObjectRotation();
            
            yield return null;
            
            Debug.Log(t);
        }
        
        StartCoroutine(RunPourAnimation());
    }
    
    private bool SnapCamera()
    {
        float step = 2f;
        
        if (Mathf.Abs(_cameraController.Pitch)  < 5f)
        {
            _cameraController.pitchInput = 0f;
            return true;
        }

        if (_cameraController.Pitch > 0)
        {
            _cameraController.pitchInput = Time.deltaTime * Mathf.Abs(_cameraController.Pitch * step);
        }
        else
        {
            _cameraController.pitchInput = -Time.deltaTime * Mathf.Abs(_cameraController.Pitch * step);
        }
        
        return false;
    }

    private void InterpolateInPosition()
    {
        _cameraController.Pitch = Mathf.Lerp(startPitch, endPitch, t);
        
    }

    private void InterpolateYRot()
    {
        _playerTransform.rotation = Quaternion.Slerp(startPlayerRotation, endPlayerRotation, t);
    }

    private void InterpolateObjectPosition()
    {
        transform.position = Vector3.Slerp(startPosition, endPosition, t);
    }

    private void InterpolateObjectRotation()
    {
        transform.rotation = Quaternion.Slerp(startObjectRotation, endObjectRotation, t);
    }
}
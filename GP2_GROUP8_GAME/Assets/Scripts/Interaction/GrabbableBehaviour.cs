//@Author: Teodor Tysklind / FutureGames / Teodor.Tysklind@FutureGames.nu

using System.Collections;
using UnityEngine;

[RequireComponent(typeof(Rigidbody))]
public class GrabbableBehaviour : MonoBehaviour, IInteractable
{
    [Tooltip("Item is drop when colliding at this force")]
    [SerializeField] private float _collisionVelocityDropThreshold = 5f;
    
    [Tooltip("If item has been displaced from crosshair, it will snap back with this speed")]
    [SerializeField] private float _displacementSnapbackSpeed = 2000f;
    
    [Tooltip(
        "If item has been displaced from crosshair, it will snap back, " +
        "and then gradually decrease in velocity until this threshold.")]
    [SerializeField] private float _slowDownThreshold = 0.1f;
    
    [Tooltip(
        "The speed will subsequently be divided by this value")]
    [SerializeField] private float _slowdownCoefficient = 5f;
    
    [Tooltip("Item will snap back upon reaching this distance from crosshair")]
    [SerializeField] private float _snapBackDistance = 0.3f;
    
    [Tooltip("Item will be dropped upon having been displaced this much from crosshair")]
    [SerializeField] private float _displacementDropDistance = 0.8f;
    
    protected bool _isGrabbed;
    private bool _isColliding;
    protected Rigidbody _rigidbody;
    private float _grabOffset;
    private const float _grabbedDrag = 0.9f;
    private float _cachedDrag;

    private Transform _playerCameraTransform;
    private Transform _grabbableTransform;
    private Vector3 _targetPosition;
    private float _distance;

    private void Awake()
    {
        _rigidbody = gameObject.GetComponent<Rigidbody>();

        _cachedDrag = _rigidbody.drag;
        _isGrabbed = false;
        _rigidbody.useGravity = true;
        _playerCameraTransform = Camera.main.transform;
        _grabbableTransform = transform;

        gameObject.layer = LayerMask.NameToLayer("Interactable");
    }

    private void Grab()
    {
        _isGrabbed = true;
        _rigidbody.useGravity = false;
        _rigidbody.freezeRotation = true;
        _rigidbody.drag = _grabbedDrag;

        AddStartBoost();
        
        GameManager.instance.player.GetComponent<CheckForInteractable>().RemoveHighlight();

        _grabOffset = Vector3.Distance(_grabbableTransform.position, _playerCameraTransform.position);
        
        _grabbableTransform.SetParent(_playerCameraTransform);
        
        StartCoroutine(UpdateGrabPosition());
    }

    protected virtual void Release()
    {
        _rigidbody.freezeRotation = false;
        _rigidbody.useGravity = true;
        _rigidbody.drag = _cachedDrag;
        
        _isGrabbed = false;
        _grabbableTransform.SetParent(null);
    }

    protected virtual IEnumerator UpdateGrabPosition()
    {
        while (_isGrabbed)
        {
            SetPosition();

            yield return null;
            
            
            if (Input.GetMouseButtonDown(0))
            {
                Release();
            }
            
        }
    }

    protected void SetPosition()
    {
        _targetPosition = _playerCameraTransform.position + (_playerCameraTransform.forward * _grabOffset);

        _distance = Vector3.Distance(_targetPosition, _grabbableTransform.position);

        if (_distance > _snapBackDistance)
        {
            _rigidbody.AddForce((_targetPosition - _grabbableTransform.position) * (Time.deltaTime * _displacementSnapbackSpeed));
        }
        else if(_rigidbody.velocity.magnitude > _slowDownThreshold)
        {
            _rigidbody.AddForce(-_rigidbody.velocity/_slowdownCoefficient);
        }
    }
    
    private void OnCollisionEnter(Collision other)
    {
        if (!_isGrabbed)
        {
            return;
        }

        if (other.relativeVelocity.magnitude > _collisionVelocityDropThreshold)
        {
            Release();
        }
    }

    private void OnCollisionStay(Collision other)
    {
        _distance = Vector3.Distance(_targetPosition, _grabbableTransform.position);

        Vector3 direction = (_targetPosition - _grabbableTransform.position).normalized;

        if (!Physics.Raycast(_grabbableTransform.position, direction, _distance))
        {
            return;
        }
        
        Release();
    }

    public virtual void Interact()
    {
        if (!_isGrabbed)
        {
            Grab();
        }
    }

    private void AddStartBoost()
    {
        Vector3 dir = _playerCameraTransform.transform.position - transform.position;
        transform.position = transform.position + dir * 0.1f;
        _rigidbody.AddForce(dir * 4);
    }
}
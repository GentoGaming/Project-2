using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using UnityEngine;
using Random = UnityEngine.Random;

namespace Scary_event_System.ScaryEventUI
{
    [Serializable]
    public class ShakingObjects
    {
        public Transform objTransform;
        [NonSerialized] public Vector3 originalPosition;
    }

    [ExecuteInEditMode]
    public class ObjectsShakeScaryEvent : ScaryEventAbstract
    {
        [Header("--------------------------------------------------------------------------------")]
        public List<ShakingObjects> shakingObjects;

        public float shakeDuration = 0f;
        public float shakeAmount = 0.7f;
        public float decreaseFactor = 1.0f;

        [Tooltip(
            "Very Important Settings, If Value is 0 Than this event will be called only once (On first Trigger). " +
            " If the value is bigger than 0 than this event will happen multiple time with value as cooldown ")]
        Vector3 _originalPos;

        private List<Vector3> _originalPositions;
        private float _shakeDuration = 0;

        public override async Task CustomUpdate()
        {
            while (true)
            {
                await Task.Delay(3);
                if (_shakeDuration > 0)
                {
                    isExecuting = true;
                    Vector3 randomNumber = Random.insideUnitSphere;
                    foreach (ShakingObjects shakingObject in shakingObjects)
                    {
                        shakingObject.objTransform.localPosition =
                            shakingObject.originalPosition + randomNumber * shakeAmount;
                    }

                    _shakeDuration -= Time.deltaTime * decreaseFactor;
                }
                else
                {
                    if (isExecuting)
                    {
                        _shakeDuration = 0f;
                        foreach (ShakingObjects shakingObject in shakingObjects)
                        {
                            shakingObject.objTransform.localPosition = shakingObject.originalPosition;
                        }

                        isExecuting = false;
                        break;
                    }
                }
            }
        }

        private void UpdatePosition()
        {
            foreach (ShakingObjects shakingObject in shakingObjects)
            {
                shakingObject.originalPosition = shakingObject.objTransform.position;
            }
        }


        public override async Task ConditionExecute()
        {
            await Task.Delay((int) (delayEvent * 1000));

            if (_shakeDuration <= 0)
            {
                UpdatePosition();
                _shakeDuration = shakeDuration;
            }

            CustomUpdate();
        }
    }
}
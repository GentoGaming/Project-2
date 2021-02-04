using System;
using System.Collections;
using System.Threading.Tasks;
using UnityEngine;

namespace Scary_event_System.ScaryEventUI
{
    [ExecuteInEditMode]
    public abstract class ScaryEventAbstract : MonoBehaviour
    {
        [Header("General Event Settings")] public float delayEvent = 0;
        public float cooldownTime = 0;
        private bool _isFirst = true, _onCooldown = false;

        protected bool isExecuting = false;
        [NonSerialized] public bool isOneTimeEvent = true;

        private void Awake()
        {
            if (cooldownTime != 0)
            {
                isOneTimeEvent = false;
            }

            MeshRenderer meshRenderer = transform.GetComponent<MeshRenderer>();
            if (meshRenderer) meshRenderer.enabled = false;
        }

        // This is the code that will change Based on each Scary Event Only .
        public abstract Task ConditionExecute();
        public abstract Task CustomUpdate();


        // One time or multiple Time event Occurence 
        public void ExecuteEvents()
        {
            if (isOneTimeEvent && _isFirst && !isExecuting)
            {
                _isFirst = false;
                ConditionExecute();
            }
            else if (!isOneTimeEvent && !isExecuting && !_onCooldown)
            {
                _onCooldown = true;
                ConditionExecute();
            }
        }

        public void ResetFirstUse()
        {
            _isFirst = true;
        }


        // Resetting cooldown of the event
        public IEnumerator ResetCooldown()
        {
            yield return new WaitForSeconds(cooldownTime);
            _onCooldown = false;
        }
    }
}
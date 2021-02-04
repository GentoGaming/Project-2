using System;
using System.Collections.Generic;
using Scary_event_System.Custom_Event_System;
using Scary_event_System.ScaryEventUI;
using UnityEditor;
using UnityEngine;

namespace Scary_event_System.Gizmo_And_Addons
{
    [ExecuteInEditMode]
    public class GenericScaryEvent : MonoBehaviour
    {
        public string eventID = null;
        private readonly List<ScaryEventAbstract> _scaryEventList = new List<ScaryEventAbstract>();

        private void Awake()
        {
            SetID();
            MonoBehaviour[] scripts = gameObject.GetComponentsInChildren<MonoBehaviour>();

            foreach (var script in scripts)
            {
                var thisScript = script as ScaryEventAbstract;
                if (thisScript == null) continue;
                _scaryEventList.Add(thisScript);
            }
        }


        private void OnEnable()
        {
            EventManager.StartListening(eventID, ListenerMethode);
        }

        private void OnDisable()
        {
            EventManager.StopListening(eventID, ListenerMethode);
        }


        private void OnTriggerEnter(Collider other)
        {
            foreach (var script in _scaryEventList)
            {
                if (other.name == "Player")
                {
                    script.ExecuteEvents();
                }

                if (!script.isOneTimeEvent)
                {
                    StartCoroutine(script.ResetCooldown());
                }
            }
        }


        private void SetID()
        {
#if UNITY_EDITOR
            if (eventID == null || eventID.Trim().Equals(""))
            {
                eventID = Guid.NewGuid().ToString();
            }
#endif
        }

        public void ListenerMethode()
        {
            foreach (var script in _scaryEventList)
            {
                script.ConditionExecute();
            }
        }


        public void AddSoundEventScript()
        {
            transform.gameObject.AddComponent<SoundScaryEvent>();
        }

        public void AddParticleEventScript()
        {
            transform.gameObject.AddComponent<ParticleScaryEvent>();
        }

        public void AddFmodScaryEventScript()
        {
            transform.gameObject.AddComponent<FmodScaryEvent>();
        }

#if UNITY_EDITOR
        public string GetEventID()
        {
            SetID();
            return eventID;
        }

        public void AddPosRotEventScript()
        {
            transform.gameObject.AddComponent<PosRotScaryEvent>();
        }


        private void CreateCubeBeforeAddingScript(String cubeName)
        {
            var objToSpawn = new GameObject(cubeName);
            objToSpawn.transform.parent = gameObject.transform;
            objToSpawn.transform.localPosition = new Vector3(0, -0.5f, 0);

            if (cubeName.Equals("FootTrail"))
            {
                objToSpawn.transform.gameObject.AddComponent<FootTrailScaryEvent>();
            }
            else if (cubeName.Equals("BloodTrail"))
            {
                objToSpawn.transform.gameObject.AddComponent<BloodTrailScaryEvent>();
            }

            Selection.activeObject = objToSpawn;
        }


        public void AddFootStepEventScript()
        {
            CreateCubeBeforeAddingScript("FootTrail");
        }


        public void AddBloodTrailEventScript()
        {
            CreateCubeBeforeAddingScript("BloodTrail");
        }

        public void AddObjectShakeEventScript()
        {
            ObjectsShakeScaryEvent cs = transform.gameObject.AddComponent<ObjectsShakeScaryEvent>();
            cs.enabled = true;
        }
#endif
    }
}
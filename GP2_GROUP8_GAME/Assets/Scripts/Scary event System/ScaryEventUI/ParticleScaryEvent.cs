using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Scary_event_System.Other;
using UnityEditor;
using UnityEngine;

namespace Scary_event_System.ScaryEventUI
{
    [Serializable]
    public class CustomParticle
    {
        [HideInInspector] public GameObject prefab;
        [HideInInspector] public float delay;
        [HideInInspector] public Transform objTransform;
    }

    public class ParticleScaryEvent : ScaryEventAbstract
    {
        [HideInInspector] public List<CustomParticle> particles = new List<CustomParticle>();
        [HideInInspector] public GameObject prefab;
        [HideInInspector] public float delay;
        [HideInInspector] public Transform objTransform;

        public override async Task ConditionExecute()
        {
            List<GameObject> gameObjects = new List<GameObject>();

            await Task.Delay((int) (delayEvent * 1000));
            particles.Sort((particle1, particle2) => particle1.delay.CompareTo(particle2.delay));
            foreach (CustomParticle particle in particles)
            {
                await Task.Delay((int) (particle.delay * 1000));
                GameObject go = Instantiate(particle.prefab, particle.objTransform, true);
                go.AddComponent<DestroySelf>();
                go.transform.localPosition = Vector3.zero;

#if UNITY_EDITOR
                gameObjects.Clear();
                foreach (Transform child in go.transform)
                {
                    gameObjects.Add(child.gameObject);
                }

                GameObject[] gos = gameObjects.ToArray();
                Selection.objects = gos;
#endif
            }
        }

        public override async Task CustomUpdate()
        {
            // Not implementing This one 
        }

        public void DeleteParticle(CustomParticle particle)
        {
            List<CustomParticle> temp = particles.ToList();
            temp.Remove(particle);
            particles = temp;
        }

        public void AddCustomParticle(Transform scrayEventObjTransform, GameObject scrayEventPrefab,
            float scrayEventDelay)
        {
            CustomParticle particle = new CustomParticle();
            particle.objTransform = scrayEventObjTransform;
            particle.prefab = scrayEventPrefab;
            particle.delay = scrayEventDelay;
            particles.Add(particle);
        }
    }
}
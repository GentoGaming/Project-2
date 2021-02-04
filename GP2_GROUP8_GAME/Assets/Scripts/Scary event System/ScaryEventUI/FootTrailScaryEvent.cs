using System.Collections.Generic;
using System.Threading.Tasks;
using UnityEditor;
using UnityEngine;

namespace Scary_event_System.ScaryEventUI
{
    [ExecuteInEditMode]
    public class FootTrailScaryEvent : ScaryEventAbstract
    {
        [Header("--------------------------------------------------------------------------------")] [SerializeField]
        private List<ParticleSystem> footTrailList = new List<ParticleSystem>();

        [SerializeField] private float delayEveryParticle = 1;


        public override async Task ConditionExecute()
        {
            await Task.Delay((int) (delayEvent * 1000));
            await CustomUpdate();
        }


        public override async Task CustomUpdate()
        {
            foreach (var particle in footTrailList)
            {
                particle.Stop();
                particle.Clear();
            }

#if UNITY_EDITOR
            GameObject[] gos = _gameObjects.ToArray();
            Selection.objects = gos;
#endif

            foreach (var particle in footTrailList)
            {
                Debug.Log("Playing");
                if (!particle.isPlaying)
                {
                    particle.Play();
                }
                else
                {
                    Debug.Log("It is playing");
                }

                await Task.Delay((int) (delayEveryParticle * 1000));
            }
        }

#if UNITY_EDITOR
        private readonly List<ParticleSystem> _tempFootTrailList = new List<ParticleSystem>();
        private readonly List<GameObject> _gameObjects = new List<GameObject>();
        private string _particleName = "FootStepParticle";
        private int _difference = 0;


        void OnDrawGizmos()
        {
            _difference = 0;
            _gameObjects.Clear();
            _tempFootTrailList.Clear();
            foreach (Transform objTransform in transform)
            {
                GameObject obj = objTransform.gameObject;
                if (obj.name == $"{_particleName}(Clone)")
                {
                    obj.transform.position = new Vector3(obj.transform.position.x, obj.transform.parent.position.y,
                        obj.transform.position.z);
                    _gameObjects.Add(obj);
                    _tempFootTrailList.Add(obj.GetComponent<ParticleSystem>());
                }
            }

            _difference = footTrailList.Count - _gameObjects.Count;

            if (_difference > 0)
            {
                for (int i = 0; i < _difference; i++)
                {
                    Instantiate(Resources.Load("Particle Effects/" + _particleName) as GameObject, transform);
                }
            }
            else if (_difference < 0)
            {
                _gameObjects.Reverse();
                for (int i = 0; i < (-1 * _difference); i++)
                {
                    DestroyImmediate(_gameObjects[i]);
                }
            }
            else
            {
                for (int i = 0; i < footTrailList.Count; i++)
                {
                    if (footTrailList[i] == null || (footTrailList[i] != _tempFootTrailList[i]))
                    {
                        footTrailList[i] = _tempFootTrailList[i];
                    }
                }
            }
        }
#endif
    }
}
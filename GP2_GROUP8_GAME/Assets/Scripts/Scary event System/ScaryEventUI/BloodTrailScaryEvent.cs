using System.Collections.Generic;
using System.Threading.Tasks;
using UnityEditor;
using UnityEngine;

namespace Scary_event_System.ScaryEventUI
{
    [ExecuteInEditMode]
    public class BloodTrailScaryEvent : ScaryEventAbstract
    {
        [SerializeField] private List<ParticleSystem> bloodTrailList = new List<ParticleSystem>();
        [SerializeField] private float delayEveryParticle = 1;


        public override async Task ConditionExecute()
        {
            await Task.Delay((int) (delayEvent * 1000));
            await CustomUpdate();
        }


        public override async Task CustomUpdate()
        {
            foreach (var particle in bloodTrailList)
            {
                particle.Stop();
                particle.Clear();
            }

#if UNITY_EDITOR
            GameObject[] gos = _gameObjects.ToArray();
            Selection.objects = gos;
#endif

            foreach (var particle in bloodTrailList)
            {
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
        private List<ParticleSystem> _tempbloodTrailList = new List<ParticleSystem>();
        private List<GameObject> _gameObjects = new List<GameObject>();
        private string _particleName = "BloodMarks";
        private int _difference = 0;


        void OnDrawGizmos()
        {
            _difference = 0;
            _gameObjects.Clear();
            _tempbloodTrailList.Clear();
            foreach (Transform objTransform in transform)
            {
                GameObject obj = objTransform.gameObject;
                if (obj.name == $"{_particleName}(Clone)")
                {
                    obj.transform.position = new Vector3(obj.transform.position.x, obj.transform.parent.position.y,
                        obj.transform.position.z);
                    _gameObjects.Add(obj);
                    _tempbloodTrailList.Add(obj.GetComponent<ParticleSystem>());
                }
            }

            _difference = bloodTrailList.Count - _gameObjects.Count;

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
                for (int i = 0; i < bloodTrailList.Count; i++)
                {
                    if (bloodTrailList[i] == null || (bloodTrailList[i] != _tempbloodTrailList[i]))
                    {
                        bloodTrailList[i] = _tempbloodTrailList[i];
                    }
                }
            }
        }
#endif
    }
}
using System.Threading.Tasks;
using UnityEngine;

namespace Scary_event_System.ScaryEventUI
{
    public class PosRotScaryEvent : ScaryEventAbstract
    {
        [Header("--------------------------------------------------------------------------------")]
        public Vector3 startPosition;

        public Vector3 startRotation;

        public Vector3 finishPosition;
        public Vector3 finishRotation;

        [HideInInspector] public Transform target;

        public bool reverse = false;


        private void OnEnable()
        {
            if (target == null) return;

            bool needsReset = target.localPosition != startPosition ||
                              target.localRotation.eulerAngles != startRotation;
            if (!needsReset) return;

            Debug.LogError("Script : Pos Rotation , The target: " + target.name + " start position is not reset");
            Debug.LogError("reset it by going  " + gameObject.name + "   Position and rotation Script --> press Reset");
            Debug.LogError("Contact @dev Maroun");
        }

        public override async Task ConditionExecute()
        {
            await Task.Delay((int) (delayEvent * 1000));
            var transform1 = target.transform;

            if (!reverse)
            {
                transform1.localPosition = finishPosition;
                transform1.localRotation = Quaternion.Euler(finishRotation);
            }

            else
            {
                transform1.localPosition = startPosition;
                transform1.localRotation = Quaternion.Euler(startRotation);
            }
        }

        public override async Task CustomUpdate()

        {
            await Task.Delay((int) (delayEvent * 1000));
            // Nothing to add in the custom Update for this event
        }
    }
}
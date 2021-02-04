using UnityEditor;
using UnityEngine;

namespace Scary_event_System.Gizmo_And_Addons
{
    public class GizmoScript : MonoBehaviour
    {
#if UNITY_EDITOR

        private void DrawEventsLabels()
        {
            var selected = Selection.activeObject;
            bool condition = this.gameObject == selected;
            
            foreach(Transform go in transform)
            {
                MeshRenderer meshRenderer = go.GetComponent<MeshRenderer>();
                var childTransform = go.transform;
                var position = childTransform.position;

                if (selected != go.gameObject)
                {
                    meshRenderer.enabled = condition;
                    Gizmos.DrawIcon(position, "Assets/Scripts/Scary event System/Gizmo And Addons/Gizmo icon/ghost.png", true);
                }
            }
            
        }

        private void OnDrawGizmos()
        {
            transform.position = Vector3.zero;
            DrawEventsLabels();
            
        }
#endif
    }
    
    
}
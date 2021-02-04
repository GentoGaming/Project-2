using System;
using System.Threading.Tasks;
using UnityEngine;

namespace Scary_event_System.Other
{
    [ExecuteInEditMode]
    public class DestroySelf : MonoBehaviour
    {
        private void OnEnable()
        {
            DestroyIfUnityEditor();
 
        }

        private async Task DestroyIfUnityEditor()
        {
            // if designer wants will let it work in non editor mode to get rid of the particles left
            #if UNITY_EDITOR
            await Task.Delay((int) (5000));
            if(gameObject) DestroyImmediate(gameObject);
            #endif
        }
    }
}

using UnityEditor;
using UnityEngine;

namespace Journal_System.Code.Scripts
{
    public class JournalNode : ScriptableObject
    {
        [SerializeField] private string imageNameT = "";
        [SerializeField] private int orderT = 999;
        private Rect _rectT = new Rect(0, 0, 600, 1000);
        [SerializeField] private Texture2D image;


        public void SetImageName(string imageName)
        {
#if UNITY_EDITOR

            Undo.RecordObject(this, "Set Image Name");
            imageNameT = imageName;
            EditorUtility.SetDirty(this);
#endif
        }


        public void SetRect(Rect rect)
        {
#if UNITY_EDITOR
            Undo.RecordObject(this, "Set Image Position");
            _rectT = rect;
            EditorUtility.SetDirty(this);
#endif
        }

        public void SetOrder(int order)
        {
#if UNITY_EDITOR
            Undo.RecordObject(this, "Set Image Order");
            orderT = order;
            EditorUtility.SetDirty(this);
#endif
        }

        public Texture2D GetImage()
        {
            return image;
        }

        public void SetImageSprite(Texture2D imageSprite)
        {
#if UNITY_EDITOR

            Undo.RecordObject(this, "Set Image Sprite");
            this.image = imageSprite;
            EditorUtility.SetDirty(this);
#endif
        }


        public int GetOrder()
        {
            return orderT;
        }

        public string GetImageName()
        {
            return imageNameT;
        }

        public Rect GetRect()
        {
            return _rectT;
        }
    }
}
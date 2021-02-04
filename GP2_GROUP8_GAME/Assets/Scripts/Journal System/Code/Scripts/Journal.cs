using System;
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;

namespace Journal_System.Code.Scripts
{
    [CreateAssetMenu(fileName = "Journal Chapter x", menuName = "Journal System / New Journal")]
    public class Journal : ScriptableObject, ISerializationCallbackReceiver
    {
        [SerializeField] private List<JournalNode> allJournalNodes = new List<JournalNode>();
#if UNITY_EDITOR
        [SerializeField] private DefaultAsset folderPath = null;
#endif

        public void OnBeforeSerialize()
        {
#if UNITY_EDITOR

            if (AssetDatabase.GetAssetPath(this) != "")
            {
                foreach (JournalNode node in GetAllNodes())
                {
                    if (AssetDatabase.GetAssetPath(node) == "")
                    {
                        AssetDatabase.AddObjectToAsset(node, this);
                    }
                }
            }
#endif
        }

        public void OnAfterDeserialize()
        {
        }


        //Creating new Journal
        public void CreatingJournal(Texture2D imageSprite)
        {
#if UNITY_EDITOR

            JournalNode newNode = CreateInstance<JournalNode>();

            newNode.name = Guid.NewGuid().ToString();
            newNode.SetImageName(imageSprite.name);
            newNode.SetImageSprite(imageSprite);

            int max = -99;

            foreach (JournalNode node in GetAllNodes())
            {
                if (node.GetOrder() > max)
                {
                    max = node.GetOrder();
                }
            }


            Undo.RegisterCreatedObjectUndo(newNode, "Created A new Journal Node");
            Undo.RecordObject(this, "Added Journal Node");


            allJournalNodes.Add(newNode);
#endif
        }


        public List<JournalNode> GetAllNodes()
        {
            return allJournalNodes;
        }


        public void DeletingJournal(JournalNode nodeToDelete)
        {
#if UNITY_EDITOR

            Undo.RecordObject(this, "Deleted Journal Node");
            allJournalNodes.Remove(nodeToDelete);
            Undo.DestroyObjectImmediate(nodeToDelete);
#endif
        }

        public bool IsFolderPathNotEmpty()
        {
#if UNITY_EDITOR

            if (folderPath != null && !folderPath.ToString().Equals(""))
            {
                return true;
            }
#endif
            return false;
        }

        public void OrderList(Vector2 widthHeight)
        {
#if UNITY_EDITOR

            allJournalNodes.Sort((p1, p2) => p1.GetOrder().CompareTo(p2.GetOrder()));
            int order = 0;
            float offset = 10;

            foreach (JournalNode journal in allJournalNodes)
            {
                if (order > 0)
                {
                    offset = offset + 300;
                }

                order += 1;
                journal.SetRect(new Rect(offset, 200, widthHeight.x, widthHeight.y));
                journal.SetOrder(order);
            }
#endif
        }
#if UNITY_EDITOR

        public DefaultAsset GetFolderPath()
        {
            return folderPath;
        }

        public void SetFolderPath(DefaultAsset path)
        {
            Undo.RecordObject(this, "Set Folder Path");
            folderPath = path;
            EditorUtility.SetDirty(this);
        }
#endif
    }
}
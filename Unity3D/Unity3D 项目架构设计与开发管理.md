# Unity3D 项目架构设计与开发管理
## Manager Of Managers 架构
- MainManager

    - EventManager

    - AudioManager

    - GUIManager

    - **PoolManager**

        维护一个已经加载到内存中但以后可能还会用到的对象的列表，这个列表包含不同类型的游戏对象或预置体

        ```csharp
        private List<GameObject> dormantObjects = new List<GameObject>();
        ```

        三个典型的操作：

        **SPAWN()**

        ```csharp
        public GameObject Spawn(GameObject go)
        {
            GameObject temp = null;
            if (dormantObjects.Count > 0)
            {
                foreach (GameObject dob in dormantObjects)
                {
                    if (dob.name == go.name)
                    {
                        temp = dob;
                        dormantObjects.Remove(temp);
                        return temp;
                    }
                }
            }

            temp = GameObject.Instantiate(go) as GameObject;
            temp.name = go.name;
            return temp;
        }
        ```

        **DESPAWN()**

        ```csharp
        public void Despawn(GameObject go)
        {
            go.transform.parent = PoolManager.transform;
            go.SetActive(false);
            dormantObjects.Add(go);
            Trim();
        }
        ```

        **TRIM()**

        ```csharp
        public void Trim()
        {
            while (dormantObjects.Count > Capacity)
            {
                GameObject dob = dormantObjects[0];
                dormantObjects.RemoveAt(0);
                Destroy(dob);
            }
        }
        ```

        **这种设计存在的问题：**

        - 这个内存池管理者没有提供加载和卸载预置体的方法，你必须在外部加载和卸载预置体

        - 只能管理所谓**暂时不用**的对象，但在游戏运行中还有很多正在使用的对象，仍需要在外部用大量的代码来进行管理

        - 内存池管理者能够管理的最大数量是设置好的，并且不是针对每一种预置体单独进行管理（可能出现某一种预置体的实力对象占了整个内存池一半的容量的情况）

        **一个更好的设计：采用层次设计**

        ```
        + PoolManager       // top pool manager
            + SpawnPool     // for a type of prefabs
                + PrefabPool // for a prefab
                    - Active instances
                    - Inactive instances
        ```

        **设计原则：**

        *PoolManager*：**单例模式**，管理多个 SpawnPools

        *SpawnPool*：定义成一个 empty object，设置它的 transform 为所有他管理的内存池中实例的 parent，并且使用 dictionary 管理多个 PrefabPools

        *PrefabPool*：为每一个预置体创建一个 PrefabPool，维护两个 list 列表，一个用于管理活跃的对象，另一个用于管理暂时不用的对象，并且在这里集中管理预置体的加载和卸载

    - **LevelManager**

        **为什么使用 LevelManager**

        Unity3D 原生自带的`LoadLevel()`方法有如下弊端：

        - 问题1：你必须知道你想加载的场景的名字或者场景的索引号，但是绝大多数情况，场景的名字或索引号在今后都可能发生变化

        - 问题2：没有一种简单的方法，在加载场景的时候传递一些参数过去，比如：你在不同关卡复用同一个场景的时候

        - 问题3：管理多个关卡的工作流不是一个简单的任务，比如：你想创建两个不同的版本，分别为DEMO和完整版，但是他们拥有不同的场景顺序，这就使得管理起来非常麻烦

        **LevelManager**设计原则：

        - 构造一个配置表

        - 创建一个新的API：`LevelManager.LoadNext()`

        在配置表中，它也可以允许为每一个关卡设置一个参数，并且这个参数会非常容易得到

        同时，你可以创建多个配置表，需要不同的版本时，只需要简单的更换加载的配置表即可

        更多的扩展用法：你可以管理两个关卡之间的一些切换效果，比如渐变、颜色变化、动画等

    - GameManager

    - **SaveManager**

        保存和加载用户偏好设置和归档文件，实现游戏的暂停和继续

    - MenuManager

    - ...

主管理者（MainManager）定制和管理子管理者（SubManager）

子管理者（SubManager）作为单例模式可以很容易的互相访问和操作
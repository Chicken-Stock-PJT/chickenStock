import MyAsset from "./ui/MyAsset";
import UserInfo from "./ui/UserInfo";

const Profile = () => {
  return (
    <div className="space-y-6">
      <UserInfo />
      <MyAsset />
    </div>
  );
};

export default Profile;

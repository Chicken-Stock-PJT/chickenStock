import apiClient from "@/shared/api/axios";

export const getUserInfo = async () => {
    const response = await apiClient.get('/members/simple-profile');
    return response.data;
};

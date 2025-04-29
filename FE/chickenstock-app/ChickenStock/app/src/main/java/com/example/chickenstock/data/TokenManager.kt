package com.example.chickenstock.data

import android.content.Context
import android.content.SharedPreferences

class TokenManager private constructor(context: Context) {
    private val prefs: SharedPreferences = context.getSharedPreferences(PREF_NAME, Context.MODE_PRIVATE)

    companion object {
        internal const val PREF_NAME = "TokenPrefs"
        private const val KEY_ACCESS_TOKEN = "access_token"
        private const val KEY_REFRESH_TOKEN = "refresh_token"
        private const val KEY_ACCESS_TOKEN_EXPIRES_IN = "access_token_expires_in"
        
        @Volatile
        private var instance: TokenManager? = null

        fun getInstance(context: Context): TokenManager {
            return instance ?: synchronized(this) {
                instance ?: TokenManager(context).also { instance = it }
            }
        }
    }

    fun saveTokens(accessToken: String, refreshToken: String, expiresIn: Long) {
        prefs.edit().apply {
            putString(KEY_ACCESS_TOKEN, accessToken)
            putString(KEY_REFRESH_TOKEN, refreshToken)
            putLong(KEY_ACCESS_TOKEN_EXPIRES_IN, System.currentTimeMillis() + expiresIn)
            apply()
        }
    }

    fun getAccessToken(): String? = prefs.getString(KEY_ACCESS_TOKEN, null)
    
    fun getRefreshToken(): String? = prefs.getString(KEY_REFRESH_TOKEN, null)
    
    fun getAccessTokenExpiresIn(): Long = prefs.getLong(KEY_ACCESS_TOKEN_EXPIRES_IN, 0)

    fun clearTokens() {
        prefs.edit().clear().apply()
    }

    fun isTokenExpired(): Boolean {
        val expiresIn = getAccessTokenExpiresIn()
        return System.currentTimeMillis() >= expiresIn
    }

    fun getSharedPreferences(): SharedPreferences = prefs
} 
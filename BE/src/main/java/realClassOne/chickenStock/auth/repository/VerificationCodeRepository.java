package realClassOne.chickenStock.auth.repository;

import java.util.Optional;

public interface VerificationCodeRepository {
    void saveCode(String email, String code);
    Optional<String> getCode(String email);
    void deleteCode(String email);

    void markVerified(String email);
    boolean isVerified(String email);
    void removeVerified(String email);
}

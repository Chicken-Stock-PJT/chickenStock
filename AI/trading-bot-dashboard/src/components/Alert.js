import React, { useState, useEffect } from 'react';

function Alert({ type, message, onClose }) {
  const [visible, setVisible] = useState(true);
  
  useEffect(() => {
    // 5초 후 자동으로 알림 닫기
    const timer = setTimeout(() => {
      setVisible(false);
      onClose();
    }, 5000);
    
    return () => clearTimeout(timer);
  }, [onClose]);
  
  if (!visible) return null;
  
  return (
    <div className={`alert alert-${type} alert-dismissible fade show`} role="alert">
      {message}
      <button 
        type="button" 
        className="btn-close" 
        onClick={() => {
          setVisible(false);
          onClose();
        }}
        aria-label="Close"
      ></button>
    </div>
  );
}

export default Alert;
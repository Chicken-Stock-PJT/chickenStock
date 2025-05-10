import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
} from "@/shared/libs/ui/alert-dialog";

interface AlertModalProps {
  isOpen: boolean;
  setIsOpen: (isOpen: boolean) => void;
  title: string | null;
  description: string;
  action: () => void;
  actionText: string;
}

const MyAlertDialog = ({
  isOpen,
  setIsOpen,
  title,
  description,
  action,
  actionText,
}: AlertModalProps) => {
  return (
    <AlertDialog open={isOpen} onOpenChange={setIsOpen}>
      <AlertDialogContent>
        <AlertDialogHeader>
          {title && <AlertDialogTitle>{title}</AlertDialogTitle>}
          <AlertDialogDescription className="text-gray-900">{description}</AlertDialogDescription>
        </AlertDialogHeader>
        <AlertDialogFooter>
          <AlertDialogAction onClick={action}>{actionText}</AlertDialogAction>
        </AlertDialogFooter>
      </AlertDialogContent>
    </AlertDialog>
  );
};

export default MyAlertDialog;
